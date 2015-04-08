module Text.Sass.Functions
  (
    SassFunctionType
  , SassFunction (..)
  , makeNativeFunction
  , clearNativeFunction
  , freeNativeFunction
  , makeNativeFunctionList
  , clearNativeFunctionList
  , freeNativeFunctionList
  ) where

import qualified Binding.Libsass            as Lib
import           Control.Monad.Loops        (whileM_)
import           Control.Monad.State.Strict
import           Foreign
import           Foreign.C
import           Text.Sass.Utils            (listEntryNotNull)
import           Text.Sass.Values

-- | Type of the function that may be used in sass source.
type SassFunctionType =
     SassValue    -- ^ Arguments of the function('SassList').
  -> IO SassValue -- ^ Result of the computation.

-- | Description of the function that may be used in sass source.
data SassFunction = SassFunction {
    funcSignature   :: String -- ^ Signature of the function, parseable by libsass.
  , funcComputation :: SassFunctionType -- ^ Main function.
}

-- | Wraps function of type 'SassFunctionType' into function that may be passed
--   to native library.
wrapFunction :: SassFunctionType -> Lib.SassFunctionFnType
wrapFunction fn args _ _ = fromNativeValue args >>= fn >>= toNativeValue

-- | Converts 'SassFunction' into native representation.
--
--   Freeing native representation is not a pleasant process - libsass frees
--   the 'Lib.SassFunctionEntry', but does not free signature. Because of that,
--   special care must be taken in order to properly deallocate the object.
--   If you don't want to pass the resulting object to Sass_Options,
--   call both 'clearNativeFunction' and then 'freeNativeFunction'. Otherwise,
--   you should call 'clearNativeFunction' BEFORE you deallocate context.
makeNativeFunction :: SassFunction -> IO Lib.SassFunctionEntry
makeNativeFunction (SassFunction sig' fn) = do
    sig <- newCString sig'
    wrapped <- Lib.mkSassFunctionFn $ wrapFunction fn
    Lib.sass_make_function sig wrapped nullPtr

-- | Releases the signature of a function entry.
clearNativeFunction :: Lib.SassFunctionEntry -> IO ()
clearNativeFunction entry = do
    sig <- Lib.sass_function_get_signature entry
    free sig

-- | Deallocates the object, but does not deallocate signature.
freeNativeFunction :: Lib.SassFunctionEntry -> IO ()
freeNativeFunction = free

-- | Converts list of 'SassFunction' into native representation.
--
--   There is analogous problem in relation to deallocation of the result as
--   with 'makeNativeFunction'. See documentation above for explanation.
makeNativeFunctionList :: [SassFunction] -> IO Lib.SassFunctionList
makeNativeFunctionList lst = do
    let len = fromIntegral $ length lst
    result <- Lib.sass_make_function_list len
    flip runStateT 0 $ forM_ lst $ \fn -> do
        idx <- get
        modify' (+1)
        entry <- liftIO $ makeNativeFunction fn
        liftIO $ Lib.sass_function_set_list_entry result idx entry
    return result

-- | Releases signatures of entries in the list.
clearNativeFunctionList :: Lib.SassFunctionList -> IO ()
clearNativeFunctionList list = do
    flip evalStateT list $ whileM_ listEntryNotNull $ do
        ptr <- get
        val <- liftIO $ peek ptr
        put (ptr `plusPtr` sizeOf val)
        liftIO $ clearNativeFunction val

-- | Frees the list and entries, without releasing signatures.
freeNativeFunctionList :: Lib.SassFunctionList -> IO ()
freeNativeFunctionList list = do
    flip evalStateT list $ whileM_ listEntryNotNull $ do
        ptr <- get
        val <- liftIO $ peek ptr
        put (ptr `plusPtr` sizeOf val)
        liftIO $ freeNativeFunction val
