-- | Provides support for user-defined functions, importers and headers.
module Text.Sass.Functions
  (
    -- * Functions
    SassFunctionType
  , SassFunction (..)
    -- * Imports and headers
  , SassImport (..)
  , SassImporterType
  , SassImporter (..)
  , SassHeaderType
  , SassHeader (..)
  , makeSourceImport
  , makePathImport
  ) where

import           Text.Sass.Values

-- | Type of the function that may be used in sass source.
type SassFunctionType =
     SassValue    -- ^ Arguments of the function ('SassList').
  -> IO SassValue -- ^ Result of the computation.

-- | Description of the function that may be used in sass source.
data SassFunction = SassFunction {
    -- | Signature of the function, parsable by libsass.
    funcSignature   :: String
    -- | Main function.
  , funcComputation :: SassFunctionType
}

-- | Represents a sass import - a sass content with additional metadata.
--
-- Even though this ADT has four fields, you may just provide either
-- 'importPath' and 'importAbsolutePath' and leave loading to the library, or
-- provide 'importSource' and do not provide 'importAbsolutePath'.
-- Nevertheless, you are free to provide all of the fields.
data SassImport = SassImport {
    -- | Path to the import, as requested by the import statement.
    importPath         :: Maybe FilePath 
    -- | Absolute path to the file.
  , importAbsolutePath :: Maybe FilePath 
    -- | Import's source.
  , importSource       :: Maybe String
    -- | Source map of the import.
  , importSourceMap    :: Maybe String
}

-- | Type of the function that acts like an importer.
--
-- You may return the empty list in order to tell libsass to handle the import by
-- itself.
type SassImporterType =
     String
     -- ^ Path to the import that needs to be loaded.
  -> String
     -- ^ Absolute path to the importing file.
  -> IO [SassImport] -- ^ Imports.

-- | Description of the importer.
data SassImporter = SassImporter {
    importerPriority :: Double           -- ^ Priority of the importer.
  , importerFunction :: SassImporterType -- ^ Main function.
}

-- | Type of the function that acts like a header.
type SassHeaderType =
       String -- ^ Absolute path to the file being processed.
    -> IO [SassImport] -- ^ Imports.

-- | Description of the header.
data SassHeader = SassHeader {
    headerPriority :: Double -- ^ Priority of the header.
  , headerFunction :: SassHeaderType -- ^ Main function.
}

-- | 'makeSourceImport' @s@ is equivalent to 'SassImport'
-- @Nothing Nothing (Just s) Nothing@.
makeSourceImport :: String -> SassImport
makeSourceImport s = SassImport Nothing Nothing (Just s) Nothing

-- | 'makePathImport' @p@ is equivalent to 'SassImport'
-- @(Just p) (Just p) Nothing Nothing@.
makePathImport :: String -> SassImport
makePathImport p = SassImport (Just p) (Just p) Nothing Nothing
