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
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Text.Sass.Internal (copyToOptions)
import           Text.Sass.Types    (SassOptions)
import           Text.Sass.Utils    (loadIntFromContext, loadStringFromContext)

-- | Represents compilation error.
data SassError = SassError {
    errorStatus  :: Int, -- ^ Compilation satus code.
    errorContext :: ForeignPtr Lib.SassContext
}

-- | Loads information about error as JSON.
errorJson :: SassError -> String
errorJson = loadStringFromContext Lib.sass_context_get_error_json . errorContext

-- | Loads error text.
errorText :: SassError -> String
errorText = loadStringFromContext Lib.sass_context_get_error_text . errorContext

-- | Loads user-friendly error message.
errorMessage :: SassError -> String
errorMessage = loadStringFromContext Lib.sass_context_get_error_message
    . errorContext

-- | Loads file where problem occured.
errorFile :: SassError -> String
errorFile = loadStringFromContext Lib.sass_context_get_error_file . errorContext

-- | Loads error source.
errorSource :: SassError -> String
errorSource = loadStringFromContext Lib.sass_context_get_error_src
    . errorContext

-- | Loads line in the file where problem occured.
errorLine :: SassError -> Int
errorLine = loadIntFromContext Lib.sass_context_get_error_line . errorContext

-- | Loads line in the file where problem occured.
errorColumn :: SassError -> Int
errorColumn = loadIntFromContext Lib.sass_context_get_error_column
    . errorContext

-- | Compiles file using specified options.
compileFile :: FilePath -- ^ Path to the file.
            -> SassOptions -- ^ Compilation options.
            -> IO (Either SassError String) -- ^ Error or output string.
compileFile path opts = withCString path $ \cpath -> do
    filectx <- Lib.sass_make_file_context cpath
    let ctx = Lib.sass_file_context_get_context filectx
    let copts = Lib.sass_file_context_get_options filectx
    copyToOptions opts copts
    status <- Lib.sass_compile_file_context filectx
    if status /= 0
        then do
            fptr <- newForeignPtr ctxFinalizer filectx
            return $ Left $ SassError (fromIntegral status) $
                castForeignPtr fptr
        else do
            cstr <- Lib.sass_context_get_output_string ctx
            !str <- peekCString cstr
            return $ Right str
    where ctxFinalizer = Lib.p_sass_delete_file_context
