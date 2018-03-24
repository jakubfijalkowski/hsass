-- | Helper functions used by Compilation module.
module Text.Sass.Compilation.Internal
  (
    newCString
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as BI
import           Foreign
import           Foreign.C                hiding (newCString)
import           Foreign.Marshal.Alloc    (mallocBytes)

-- | Copies 'ByteString' to newly allocated 'CString'. The result must be
-- | explicitly freed using 'free' or 'finalizerFree'.
newCString :: ByteString -> IO CString
newCString (BI.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    BI.memcpy buf (p `plusPtr` o) (fromIntegral l)
    pokeByteOff buf l (0::Word8)
    return $ castPtr buf
