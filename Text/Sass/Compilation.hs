{-# LANGUAGE BangPatterns #-}
module Text.Sass.Compilation
  (
    SassError (errorStatus)
  , errorJson
  , errorText
  , errorMessage
  , errorFile
  , errorSource
  , errorLine
  , errorColumn
  , compileFile
  ) where

import qualified Binding.Libsass    as Lib
import           Control.Monad      ((>=>))
import           Foreign
import           Foreign.C
import           Text.Sass.Internal (copyToOptions)
import           Text.Sass.Types    (SassOptions)

-- | Represents compilation error.
data SassError = SassError {
    errorStatus  :: Int, -- ^ Compilation satus code.
    errorContext :: ForeignPtr Lib.SassContext
}

-- | Loads specified property from context and converts it to desired type.
loadFromError :: (Ptr Lib.SassContext -> IO a) -- ^ Accessor function.
              -> (a -> IO b) -- ^ Conversion method.
              -> SassError -- ^ Pointer to context.
              -> IO b -- ^ Result.
loadFromError get conv err = withForeignPtr ptr $ get >=> conv
    where ptr = errorContext err

-- | Equivalent to @'loadFromError' 'get' 'peekCString' 'err'@.
loadStringFromError :: (Ptr Lib.SassContext -> IO CString) -- ^ Accessor function.
                    -> SassError -- ^ Pointer to context.
                    -> IO String -- ^ Result.
loadStringFromError get = loadFromError get peekCString

-- | Equivalent to @'loadFromError' 'get' 'fromInteger' 'err'@.
loadIntFromError :: (Integral a)
                 => (Ptr Lib.SassContext -> IO a) -- ^ Accessor function.
                 -> SassError -- ^ Pointer to context.
                 -> IO Int -- ^ Result.
loadIntFromError get = loadFromError get (return.fromIntegral)

-- | Loads information about error as JSON.
errorJson :: SassError -> IO String
errorJson = loadStringFromError Lib.sass_context_get_error_json

-- | Loads error text.
errorText :: SassError -> IO String
errorText = loadStringFromError Lib.sass_context_get_error_text

-- | Loads user-friendly error message.
errorMessage :: SassError -> IO String
errorMessage = loadStringFromError Lib.sass_context_get_error_message

-- | Loads file where problem occured.
errorFile :: SassError -> IO String
errorFile = loadStringFromError Lib.sass_context_get_error_file

-- | Loads error source.
errorSource :: SassError -> IO String
errorSource = loadStringFromError Lib.sass_context_get_error_src

-- | Loads line in the file where problem occured.
errorLine :: SassError -> IO Int
errorLine = loadIntFromError Lib.sass_context_get_error_line

-- | Loads line in the file where problem occured.
errorColumn :: SassError -> IO Int
errorColumn = loadIntFromError Lib.sass_context_get_error_column

-- | Compiles file using specified options.
compileFile :: FilePath -- ^ Path to the file.
            -> SassOptions -- ^ Compilation options.
            -> IO (Either SassError String) -- ^ Error or output string.
compileFile path opts = withCString path $ \cpath -> do
    -- Makes an assumption, that Sass_File_Context inherits from Sass_Context
    -- and Sass_Options.
    filectx <- Lib.sass_make_file_context cpath
    let ctx = castPtr filectx
    let copts = castPtr filectx
    copyToOptions opts copts
    status <- Lib.sass_compile_file_context filectx
    if status /= 0
        then do
            fptr <- newForeignPtr ctxFinalizer filectx
            return $ Left $
                SassError (fromIntegral status) (castForeignPtr fptr)
        else do
            cstr <- Lib.sass_context_get_output_string ctx
            !str <- peekCString cstr
            Lib.sass_delete_file_context filectx
            return $ Right str
    where ctxFinalizer = Lib.p_sass_delete_file_context
