module Text.Sass.Utils where

import           Data.List        (intercalate)
import           Foreign.C
import           System.FilePath  (searchPathSeparator)

-- | Concatenates list of paths, separating entries with appropriate character.
concatPaths :: [FilePath] -> FilePath
concatPaths = intercalate [searchPathSeparator]

-- | 'withOptionalCString' @str action@, if @str@ is 'Nothing', @action@ is not
--   invoked, otherwise behaves like 'withCString'.
withOptionalCString :: Maybe String -> (CString -> IO ()) -> IO ()
withOptionalCString (Just str) action = withCString str action
withOptionalCString Nothing _ = return ()
