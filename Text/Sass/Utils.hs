-- | Helper functions. This module is internal and should not be used in
-- production code.
{-# LANGUAGE ViewPatterns #-}
module Text.Sass.Utils
  (
    -- * Interoperation with C API
    withOptionalCString
  , listEntryNotNull
  , loopCList
  , copyToCList
    -- * Other helpers
  , concatPaths
  , arrayRange
  ) where

import           Control.Monad.Loops        (whileM_)
import           Control.Monad.State.Strict hiding (sequence)
import           Data.List                  (intercalate)
import           Foreign
import           Foreign.C
import           System.FilePath            (searchPathSeparator)

-- | 'withOptionalCString' @str action@, if @str@ is 'Nothing', @action@ is not
-- invoked, otherwise behaves like 'withCString'.
withOptionalCString :: Maybe String -> (CString -> IO ()) -> IO ()
withOptionalCString (Just str) action = withCString str action
withOptionalCString Nothing _ = return ()

-- | Checks if the pointer in state points to non-null location.
listEntryNotNull :: (Monad m, MonadIO m) => StateT (Ptr (Ptr a)) m Bool
listEntryNotNull = do
    ptr <- get
    val <- liftIO $ peek ptr
    return $ val /= nullPtr

-- | 'loopCArray' @action list@ calls @action@ over each element of @list@ that
-- is C array with NULL signifying end of array.
loopCList :: (Monad m, MonadIO m) => (Ptr a -> m ()) -> Ptr (Ptr a) -> m ()
loopCList action list =
    flip evalStateT list $ whileM_ listEntryNotNull $ do
        ptr <- get
        val <- liftIO $ peek ptr
        modify' (`plusPtr` sizeOf val)
        lift $ action val

-- | Copies converted list of elements to new C array.
copyToCList :: (Num size, Enum size)
            => (CSize -> IO list) -- ^ List creator.
            -> (a -> IO element) -- ^ Conversion function.
            -> (list -> size -> element -> IO ()) -- ^ Set element function.
            -> [a] -- ^ Elements.
            -> IO list
copyToCList create convert set list = do
    let len = length list
    result <- create $ fromIntegral len
    zipWithM_ (addToList result) [0..(fromIntegral len) - 1] list
    return result
    where
        addToList lst idx = convert >=> set lst idx

-- | Concatenates list of paths, separating entries with appropriate character.
concatPaths :: [FilePath] -> FilePath
concatPaths = intercalate [searchPathSeparator]

-- | Generates array indices for array of specified length.
arrayRange :: (Num a, Integral a, Enum a) => a -> [a]
arrayRange 0 = []
arrayRange l = [0..l - 1]
