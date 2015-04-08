module Text.Sass.Types
  (
    SassOptions(..),
    Lib.SassOutputStyle(..)
  ) where

import qualified Binding.Libsass    as Lib
import           Data.Default.Class

-- | Describes options used by libsass during compilation.
data SassOptions = SassOptions {
    -- | Precision of fractional numbers.
    sassPrecision         :: Int,
    -- | Output style for the generated css code.
    sassOutputStyle       :: Lib.SassOutputStyle,
    -- | Emit comments in the generated CSS indicating the corresponding source
    --   line.
    sassSourceComments    :: Bool,
    -- | Embed sourceMappingUrl as data uri.
    sassSourceMapEmbed    :: Bool,
    -- | Embed include contents in maps.
    sassSourceMapContents :: Bool,
    -- | Disable sourceMappingUrl in css output.
    sassOmitSourceMapUrl  :: Bool,
    -- | Treat source_string as sass (as opposed to scss).
    sassIsIndentedSyntax  :: Bool,
    -- | String to be used for indentation.
    sassIndent            :: String,
    -- | String to be used to for line feeds.
    sassLinefeed          :: String,
    -- | The input path used for source map generation. It can be used to
    --   define something with string compilation or to overload the input file
    --   path.
    sassInputPath         :: Maybe FilePath,
    -- | The output path used for source map generation.
    sassOutputPath        :: Maybe FilePath,
    -- | Paths used to load plugins by libsass.
    sassPluginPaths       :: Maybe [FilePath],
    -- | Paths used to resolve @include.
    sassIncludePaths      :: Maybe [FilePath],
    -- | Path to source map file. Enables source map generation and is used to
    --   create sourceMappingUrl
    sassSourceMapFile     :: Maybe FilePath,
    -- | Directly inserted in source maps.
    sassSourceMapRoot     :: Maybe String
}

instance Default SassOptions where
    def = SassOptions {
        sassPrecision         = 5,
        sassOutputStyle       = Lib.SassStyleNested,
        sassSourceComments    = False,
        sassSourceMapEmbed    = False,
        sassSourceMapContents = False,
        sassOmitSourceMapUrl  = False,
        sassIsIndentedSyntax  = False,
        sassIndent            = "  ",
        sassLinefeed          = "\n",
        sassInputPath         = Nothing,
        sassOutputPath        = Nothing,
        sassPluginPaths       = Nothing,
        sassIncludePaths      = Nothing,
        sassSourceMapFile     = Nothing,
        sassSourceMapRoot     = Nothing
    }
