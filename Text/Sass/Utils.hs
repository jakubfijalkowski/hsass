module Text.Sass.Utils where

import           Control.Monad.State.Strict
import           Data.List                  (intercalate)
import           Foreign
import           Foreign.C
import           System.FilePath            (searchPathSeparator)

-- | Concatenates list of paths, separating entries with appropriate character.
concatPaths :: [FilePath] -> FilePath
concatPaths = intercalate [searchPathSeparator]

-- | 'withOptionalCString' @str action@, if @str@ is 'Nothing', @action@ is not
--   invoked, otherwise behaves like 'withCString'.
withOptionalCString :: Maybe String -> (CString -> IO ()) -> IO ()
withOptionalCString (Just str) action = withCString str action
withOptionalCString Nothing _ = return ()

-- | Checks if the pointer in state points to non-null location.
listEntryNotNull :: StateT (Ptr (Ptr a)) IO Bool
listEntryNotNull = do
    ptr <- get
    val <- liftIO $ peek ptr
    return $ val /= nullPtr
