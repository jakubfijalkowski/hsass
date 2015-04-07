module Text.Sass.Utils where

import           Data.List        (intercalate)
import           Foreign
import           Foreign.C
import           System.FilePath  (searchPathSeparator)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Binding.Libsass as Lib

-- | Concatenates list of paths, separating entries with appropriate character.
concatPaths :: [FilePath] -> FilePath
concatPaths = intercalate [searchPathSeparator]

-- | 'withOptionalCString' @str action@, if @str@ is 'Nothing', then 'nullPtr'
--   is passed to the @action@, otherwise behaves like 'withCString'.
withOptionalCString :: Maybe String -> (CString -> IO ()) -> IO ()
withOptionalCString (Just str) action = withCString str action
withOptionalCString Nothing action = action nullPtr

-- | 'unsafePeekString' is equivalent to
--   @'unsafePerformIO' . 'peekCString' . 'unsafePerformIO'@.
unsafePeekString :: IO CString -> String
unsafePeekString = unsafePerformIO . peekCString . unsafePerformIO

-- | 'unsafeToInt' is equivalent to @'fromIntegral' . 'unsafePerformIO'@.
unsafeToInt :: IO CSize -> Int
unsafeToInt = fromIntegral . unsafePerformIO

-- | Loads specified property from context.
loadStringFromContext :: (Ptr Lib.SassContext -> IO CString) -- ^ Accessor.
                      -> Ptr Lib.SassContext -- ^ Context.
                      -> String -- ^ Value.
loadStringFromContext action = unsafePeekString . action

-- | Loads specified property from context.
loadIntFromContext :: (Ptr Lib.SassContext -> IO CSize) -- ^ Accessor.
                      -> Ptr Lib.SassContext -- ^ Context.
                      -> Int -- ^ Value.
loadIntFromContext action = unsafeToInt . action