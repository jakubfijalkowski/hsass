-- | Compilation options.
module Text.Sass.Options
  (
    SassOptions (..)
  , defaultSassOptions
  , Lib.SassOutputStyle (..)
  ) where

import qualified Bindings.Libsass    as Lib
import           Data.Default.Class
import           Text.Sass.Functions

-- | Describes compilation options. With the exception of
-- 'sassStripEncodingInfo', these correspond to the compilation options of
-- libsass.
data SassOptions = SassOptions {
    -- | Precision of fractional numbers.
    sassPrecision         :: Int
    -- | Output style for the generated css code.
  , sassOutputStyle       :: Lib.SassOutputStyle
    -- | Emit comments in the generated CSS indicating the corresponding source
    -- line.
  , sassSourceComments    :: Bool
    -- | Embed sourceMappingUrl as data uri.
  , sassSourceMapEmbed    :: Bool
    -- | Embed include contents in maps.
  , sassSourceMapContents :: Bool
    -- | Disable sourceMappingUrl in css output.
  , sassOmitSourceMapUrl  :: Bool
    -- | Treat source_string as sass (as opposed to scss).
  , sassIsIndentedSyntax  :: Bool
    -- | String to be used for indentation.
  , sassIndent            :: String
    -- | String to be used to for line feeds.
  , sassLinefeed          :: String
    -- | The input path used for source map generation. It can be used to
    -- define something with string compilation or to overload the input file
    -- path.
  , sassInputPath         :: Maybe FilePath
    -- | The output path used for source map generation.
  , sassOutputPath        :: Maybe FilePath
    -- | Paths used to load plugins by libsass.
  , sassPluginPaths       :: Maybe [FilePath]
    -- | Paths used to resolve @include.
  , sassIncludePaths      :: Maybe [FilePath]
    -- | Path to source map file. Enables source map generation and is used to
    -- create sourceMappingUrl
  , sassSourceMapFile     :: Maybe FilePath
    -- | Directly inserted in source maps.
  , sassSourceMapRoot     :: Maybe String
    -- | List of user-supplied functions that may be used in sass files.
  , sassFunctions         :: Maybe [SassFunction]
    -- | List of user-supplied functions that provide "headers" for sass files.
    -- Header is injected at the beginning of a file which name is passed as
    -- the first argument of importer.
  , sassHeaders           :: Maybe [SassHeader]
    -- | List of user-supplied functions that resolve @import directives.
  , sassImporters         :: Maybe [SassImporter]
    -- | Remove @\@charset \"UTF-8\";\\n@ or byte-order mark from CSS output,
    -- if present.
  , sassStripEncodingInfo :: Bool
}

-- | The default 'SassOptions':
--
-- * 'sassPrecision' = 5
-- * 'sassOutputStyle' = 'Lib.SassStyleNested'
-- * 'sassIndent' = two spaces
-- * 'sassLinefeed' = @"\\n"@
-- * All other fields default to 'False' or 'Nothing'.
defaultSassOptions :: SassOptions
defaultSassOptions = SassOptions
  { sassPrecision         = 5
  , sassOutputStyle       = Lib.SassStyleNested
  , sassSourceComments    = False
  , sassSourceMapEmbed    = False
  , sassSourceMapContents = False
  , sassOmitSourceMapUrl  = False
  , sassIsIndentedSyntax  = False
  , sassIndent            = "  "
  , sassLinefeed          = "\n"
  , sassInputPath         = Nothing
  , sassOutputPath        = Nothing
  , sassPluginPaths       = Nothing
  , sassIncludePaths      = Nothing
  , sassSourceMapFile     = Nothing
  , sassSourceMapRoot     = Nothing
  , sassFunctions         = Nothing
  , sassHeaders           = Nothing
  , sassImporters         = Nothing
  , sassStripEncodingInfo = False
  }

-- | 'def' = 'defaultSassOptions'
instance Default SassOptions where
    def = defaultSassOptions
