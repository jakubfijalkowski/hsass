module Text.Sass.Functions.Internal
  (
    makeNativeFunction
  , clearNativeFunction
  , freeNativeFunction
  , makeNativeFunctionList
  , clearNativeFunctionList
  , freeNativeFunctionList
  ) where

import qualified Binding.Libsass            as Lib
import           Control.Monad.State.Strict
import           Foreign
import           Foreign.C
import           Text.Sass.Functions
import           Text.Sass.Utils            (loopCList)
import           Text.Sass.Values.Internal

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
    zipWithM_ (addToList result) lst [0..len - 1]
    return result
    where
        addToList list fn idx = do
            entry <- makeNativeFunction fn
            Lib.sass_function_set_list_entry list idx entry

-- | Releases signatures of entries in the list.
clearNativeFunctionList :: Lib.SassFunctionList -> IO ()
clearNativeFunctionList = loopCList clearNativeFunction

-- | Frees the list and entries, without releasing signatures.
freeNativeFunctionList :: Lib.SassFunctionList -> IO ()
freeNativeFunctionList = loopCList freeNativeFunction
