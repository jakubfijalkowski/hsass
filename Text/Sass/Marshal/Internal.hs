-- | Helper functions used by Compilation module.
module Text.Sass.Marshal.Internal
  (
    peekUTF8CString,
    newUTF8CString,
    withUTF8CString,
    newCStringFromBS
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as BI
import           Foreign
import           Foreign.C
import           Foreign.Marshal.Alloc    (mallocBytes)

import qualified GHC.Foreign              as GHC
import qualified GHC.IO.Encoding          as E

-- | Marshal a NUL terminated C string (UTF-8 encoded) into Haskell string.
-- It is equivalent to 'peekCString' but with different encoding.
peekUTF8CString :: CString -> IO String
peekUTF8CString = GHC.peekCString E.utf8

-- | Marshal a Haskell string into a NUL terminated C string (UTF-8 encoded).
-- It is equivalent to 'newCString' but with different encoding.
newUTF8CString :: String -> IO CString
newUTF8CString = GHC.newCString E.utf8

-- | Marshal a Haskell string into a C string (i.e., character array)
-- in temporary storage, with explicit length information.
-- It is equivalent to 'withCString' but with different encoding.
withUTF8CString :: String -> (CString -> IO a) -> IO a
withUTF8CString = GHC.withCString E.utf8

-- | Copies 'ByteString' to newly allocated 'CString'. The result must be
-- explicitly freed using 'free' or 'finalizerFree'.
newCStringFromBS :: ByteString -> IO CString
newCStringFromBS (BI.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    BI.memcpy buf (p `plusPtr` o) (fromIntegral l)
    pokeByteOff buf l (0::Word8)
    return $ castPtr buf
