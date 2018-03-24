-- | Compilation of sass source or sass files.
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Sass.Compilation
  (
    -- * Compilation
    compileFile
  , compileString
  , compileByteString
    -- * Results
  , SassExtendedResult
  , StringResult
  , ExtendedResult
  , ExtendedResultBS
  , resultString
  , resultIncludes
  , resultSourcemap
    -- * Error reporting
  , SassError
  , errorStatus
  , errorJson
  , errorText
  , errorMessage
  , errorFile
  , errorSource
  , errorLine
  , errorColumn
  ) where

import qualified Bindings.Libsass               as Lib
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B.C8
import qualified Data.ByteString.Unsafe         as B
import           Data.List                      (stripPrefix)
import           Data.Maybe                     (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative            ((<$>))
#endif
import           Control.Monad                  (forM, (>=>))
import           Foreign
import           Foreign.C
import qualified Text.Sass.Compilation.Internal as CI
import           Text.Sass.Internal             hiding (newCString)
import           Text.Sass.Options

-- | Represents compilation error.
data SassError = SassError {
    errorStatus  :: Int, -- ^ Compilation satus code.
    errorContext :: ForeignPtr Lib.SassContext
}

-- | Represents extended result - compiled string (or other string-like type,
-- eg. 'ByteString') with a list of includes and a source map.
--
-- Subject to name change in future.
data SassExtendedResult a = SassExtendedResult {
    resultString  :: a, -- ^ Compiled string.
    resultContext :: ForeignPtr Lib.SassContext
}

-- | Result of compilation - 'Either' 'SassError' or a compiled string.
--
-- Subject to name change in future.
type StringResult = IO (Either SassError String)

-- | Result of compilation - 'Either' 'SassError' or extended results - a
-- compiled string with a list of included files and a source map.
--
-- Subject to name change in future.
type ExtendedResult = IO (Either SassError (SassExtendedResult String))
--
-- | Result of compilation - 'Either' 'SassError' or extended results - a
-- compiled 'ByteString' with a list of included files and a source map.
--
-- Subject to name change in future.
type ExtendedResultBS = IO (Either SassError (SassExtendedResult ByteString))

-- | Typeclass that allows multiple results from compilation functions.
--
-- Currently, only three types are supported - 'String', 'ByteString' and
-- @'SassExtendedResult' a@ (where a is something that is an instance of
-- 'SassResult').  The first provides only a compiled string, the latter one
-- gives access to a list of included files and a source map (if available).
class SassResult a where
    toSassResult :: Bool -> ForeignPtr Lib.SassContext -> IO a

instance Show SassError where
    show (SassError s _) =
        "SassError: cannot compile provided source, error status: " ++ show s

instance Eq SassError where
    (SassError s1 _) == (SassError s2 _) = s1 == s2

instance Show (SassExtendedResult a) where
    show _ = "SassExtendedResult"

-- | Only compiled code.
instance SassResult String where
    toSassResult stripEncoding ptr = withForeignPtr ptr $ \ctx -> do
        result <- Lib.sass_context_get_output_string ctx
        !result' <- peekCString result
        return $ if stripEncoding then strip result' else result'
     where
        strip s
            | Just stripped <- stripPrefix "@charset \"UTF-8\";\n" s = stripped
            | Just stripped <- stripPrefix "\65279" s = stripped
            | otherwise = s

-- | Only compiled code (UTF-8 encoding).
instance SassResult ByteString where
    toSassResult stripEncoding ptr = withForeignPtr ptr $ \ctx -> do
        result <- Lib.sass_context_get_output_string ctx
        !result' <- B.packCString result
        return $ if stripEncoding then strip result' else result'
      where
        strip s
            | Just stripped <- stripCharset s = stripped
            | Just stripped <- stripBom s = stripped
            | otherwise = s
        stripCharset = stripPrefixBS (B.C8.pack "@charset \"UTF-8\";\n")
        stripBom = stripPrefixBS (B.C8.pack "\239\187\191")
        stripPrefixBS bs1 bs2
            | bs1 `B.C8.isPrefixOf` bs2 = Just (B.unsafeDrop (B.length bs1) bs2)
            | otherwise = Nothing

-- | Compiled code with includes and a source map.
instance (SassResult a) => SassResult (SassExtendedResult a) where
    toSassResult stripEncoding ptr = do
        str <- toSassResult stripEncoding ptr
        return $ SassExtendedResult str ptr

-- | Loads specified property from a context and converts it to desired type.
loadFromError :: (Ptr Lib.SassContext -> IO a) -- ^ Accessor function.
              -> (a -> IO b) -- ^ Conversion method.
              -> SassError -- ^ Pointer to context.
              -> IO b -- ^ Result.
loadFromError get conv err = withForeignPtr ptr $ get >=> conv
    where ptr = errorContext err

-- | Equivalent of @'loadFromError' 'get' 'peekCString' 'err'@.
loadStringFromError
    :: (Ptr Lib.SassContext -> IO CString) -- ^ Accessor function.
    -> SassError -- ^ Pointer to context.
    -> IO String -- ^ Result.
loadStringFromError get = loadFromError get peekCString

-- | Equivalent of @'loadFromError' 'get' 'fromInteger' 'err'@.
loadIntFromError :: (Integral a)
                 => (Ptr Lib.SassContext -> IO a) -- ^ Accessor function.
                 -> SassError -- ^ Pointer to context.
                 -> IO Int -- ^ Result.
loadIntFromError get = loadFromError get (return.fromIntegral)

-- | Loads information about an error as JSON.
errorJson :: SassError -> IO String
errorJson = loadStringFromError Lib.sass_context_get_error_json

-- | Loads an error text.
errorText :: SassError -> IO String
errorText = loadStringFromError Lib.sass_context_get_error_text

-- | Loads a user-friendly error message.
errorMessage :: SassError -> IO String
errorMessage = loadStringFromError Lib.sass_context_get_error_message

-- | Loads a filename where problem occured.
errorFile :: SassError -> IO String
errorFile = loadStringFromError Lib.sass_context_get_error_file

-- | Loads an error source.
errorSource :: SassError -> IO String
errorSource = loadStringFromError Lib.sass_context_get_error_src

-- | Loads a line in the file where problem occured.
errorLine :: SassError -> IO Int
errorLine = loadIntFromError Lib.sass_context_get_error_line

-- | Loads a line in the file where problem occured.
errorColumn :: SassError -> IO Int
errorColumn = loadIntFromError Lib.sass_context_get_error_column

-- | Loads a list of files that have been included during compilation.
resultIncludes :: SassExtendedResult a -> IO [String]
resultIncludes ex = withForeignPtr (resultContext ex) $ \ctx -> do
    lst <- Lib.sass_context_get_included_files ctx
    len <- Lib.sass_context_get_included_files_size ctx
    forM (arrayRange $ fromIntegral len) (peekElemOff lst >=> peekCString)

-- | Loads a source map if it was generated by libsass.
resultSourcemap :: SassExtendedResult a -> IO (Maybe String)
resultSourcemap ex = withForeignPtr (resultContext ex) $ \ctx -> do
    cstr <- Lib.sass_context_get_source_map_string ctx
    if cstr == nullPtr
        then return Nothing
        else Just <$> peekCString cstr

-- | Common code for 'compileFile' and 'compileString'.
compileInternal :: (SassResult b)
                => CString -- ^ String that will be passed to 'make context'.
                -> SassOptions
                -> (CString -> IO (Ptr a)) -- ^ Make context.
                -> (Ptr a -> IO CInt) -- ^ Compile context.
                -> FinalizerPtr a -- ^ Context finalizer.
                -> IO (Either SassError b)
compileInternal str opts make compile finalizer = do
    -- Makes an assumption, that Sass_*_Context inherits from Sass_Context
    -- and Sass_Options.
    context <- make str
    let opts' = castPtr context
    copyOptionsToNative opts opts'
    status <- withFunctions opts opts' $ compile context
    fptr <- castForeignPtr <$> newForeignPtr finalizer context
    if status /= 0
        then return $ Left $
            SassError (fromIntegral status) fptr
        else do
            result <- toSassResult (sassStripEncodingInfo opts) fptr
            return $ Right result

-- | Compiles a file using specified options.
compileFile :: SassResult a
            => FilePath -- ^ Path to the file.
            -> SassOptions -- ^ Compilation options.
            -> IO (Either SassError a) -- ^ Error or output string.
compileFile path opts = withCString path $ \cpath ->
    compileInternal cpath opts
        Lib.sass_make_file_context
        Lib.sass_compile_file_context
        Lib.p_sass_delete_file_context

-- | Compiles raw Sass content using specified options.
compileString :: SassResult a
              => String -- ^ String to compile.
              -> SassOptions -- ^ Compilation options.
              -> IO (Either SassError a) -- ^ Error or output string.
compileString str opts = do
    cdata <- newCString str
    compileInternal cdata opts
        Lib.sass_make_data_context
        Lib.sass_compile_data_context
        Lib.p_sass_delete_data_context

-- | Compiles raw Sass content using specified options.
compileByteString :: SassResult a
                  => ByteString -- ^ String to compile.
                  -> SassOptions -- ^ Compilation options.
                  -> IO (Either SassError a) -- ^ Error or output string.
compileByteString str opts = do
    cdata <- CI.newCString str
    compileInternal cdata opts
        Lib.sass_make_data_context
        Lib.sass_compile_data_context
        Lib.p_sass_delete_data_context
