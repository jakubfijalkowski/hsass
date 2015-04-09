{-# LANGUAGE ViewPatterns #-}
module Text.Sass.Utils where

import           Control.Monad.Loops        (whileM_)
import           Control.Monad.State.Strict hiding (sequence)
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
listEntryNotNull :: (Monad m, MonadIO m) => StateT (Ptr (Ptr a)) m Bool
listEntryNotNull = do
    ptr <- get
    val <- liftIO $ peek ptr
    return $ val /= nullPtr

-- | 'loopCArray' @action list@ calls @action@ over each element of @list@ that
--   is C array with NULL signifying end of array.
loopCList :: (Monad m, MonadIO m) => (Ptr a -> m ()) -> Ptr (Ptr a) -> m ()
loopCList action list =
    flip evalStateT list $ whileM_ listEntryNotNull $ do
        ptr <- get
        val <- liftIO $ peek ptr
        modify' (`plusPtr` sizeOf val)
        lift $ action val
