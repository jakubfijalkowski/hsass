{-# LANGUAGE LambdaCase #-}
module Text.Sass.Functions.Internal
  (
    -- * Functions
    wrapFunction
  , makeNativeFunction
  , clearNativeFunction
  , freeNativeFunction
  , makeNativeFunctionList
  , clearNativeFunctionList
  , freeNativeFunctionList
    -- * Imports and headers
  , wrapImporter
  , makeNativeImport
  , freeNativeImport
  , makeNativeImportList
  , freeNativeImportList
  , makeNativeImporter
  , freeNativeImporter
  , makeNativeImporterList
  , freeNativeImporterList
  ) where

import qualified Bindings.Libsass          as Lib
import           Foreign
import           Foreign.C
import           Text.Sass.Functions
import           Text.Sass.Utils
import           Text.Sass.Values.Internal

-- | Wraps function of type 'SassFunctionType' into function that may be passed
-- to native library.
wrapFunction :: SassFunctionType -> Lib.SassFunctionFnType
wrapFunction fn args _ _ = fromNativeValue args >>= fn >>= toNativeValue

-- | Converts 'SassFunction' into native representation.
--
-- Freeing native representation is not a pleasant process - libsass frees
-- the 'Lib.SassFunctionEntry', but does not free signature. Because of that,
-- special care must be taken in order to properly deallocate the object.
-- If you don't want to pass the resulting object to Sass_Options,
-- call both 'clearNativeFunction' and then 'freeNativeFunction'. Otherwise,
-- you should call 'clearNativeFunction' BEFORE you deallocate context.
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

-- | Converts list of 'SassFunction's into native representation.
--
-- There is analogous problem in relation to deallocation of the result as
-- with 'makeNativeFunction'. See documentation above for explanation.
makeNativeFunctionList :: [SassFunction] -> IO Lib.SassFunctionList
makeNativeFunctionList =
    copyToCList Lib.sass_make_function_list makeNativeFunction pokeElemOff

-- | Releases signatures of entries in the list.
clearNativeFunctionList :: Lib.SassFunctionList -> IO ()
clearNativeFunctionList = loopCList clearNativeFunction

-- | Frees the list and entries, without releasing signatures.
freeNativeFunctionList :: Lib.SassFunctionList -> IO ()
freeNativeFunctionList = loopCList freeNativeFunction

-- | Wraps function of type 'SassImporterType'.
wrapImporter :: SassImporterType -> Lib.SassImporterFnType
wrapImporter fn url _ _ = peekCString url >>= fn >>= \case
    [] -> return nullPtr
    xs -> makeNativeImportList xs

-- | Converts 'SassImport' into native representation.
makeNativeImport :: SassImport -> IO Lib.SassImportEntry
makeNativeImport el = do
    path <- maybeNew newCString $ importPath el
    base <- maybeNew newCString $ importPath el
    source <- maybeNew newCString $ importSource el
    srcmap <- maybeNew newCString $ importSourceMap el
    Lib.sass_make_import path base source srcmap

-- | Frees native representation of 'SassImport'.
freeNativeImport :: Lib.SassImportEntry -> IO ()
freeNativeImport = Lib.sass_delete_import

-- | Converts list of 'SassImport's into native representation.
makeNativeImportList :: [SassImport] -> IO Lib.SassImportList
makeNativeImportList =
    copyToCList Lib.sass_make_import_list makeNativeImport pokeElemOff

-- | Frees native representation of list of 'SassEntry', including entries.
freeNativeImportList :: Lib.SassImportList -> IO ()
freeNativeImportList = Lib.sass_delete_import_list

-- | Converts 'SassImporter' into native representation.
makeNativeImporter :: SassImporter -> IO Lib.SassImporterEntry
makeNativeImporter (SassImporter p func) = do
    func' <- Lib.mkSassImporterFn $ wrapImporter func
    Lib.sass_make_importer func' (realToFrac p) nullPtr

-- | Frees native representation of 'SassImporter'.
freeNativeImporter :: Lib.SassImporterEntry -> IO ()
freeNativeImporter = Lib.sass_delete_importer

-- | Makes native representation of list of 'SassImporter's.
makeNativeImporterList :: [SassImporter] -> IO Lib.SassImporterList
makeNativeImporterList =
    copyToCList Lib.sass_make_importer_list makeNativeImporter pokeElemOff

-- | Frees list of native representations of 'SassImporter's.
--
-- Libsass does not provide function to free this kind of objects, but we
-- provide it just in case.
freeNativeImporterList :: Lib.SassImporterList -> IO ()
freeNativeImporterList lst = loopCList freeNativeImporter lst >> free lst
