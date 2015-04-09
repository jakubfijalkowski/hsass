{-# LANGUAGE BangPatterns #-}
module Text.Sass.Options
  (
    SassOptions (..)
  , copyOptionsToNative
  , withFunctions
  , Lib.SassOutputStyle (..)
  ) where

import qualified Binding.Libsass     as Lib
import           Control.Applicative ((<$>))
import           Data.Default.Class
import           Foreign
import           Foreign.C
import           Text.Sass.Functions
import           Text.Sass.Utils

-- | Describes options used by libsass during compilation.
data SassOptions = SassOptions {
    -- | Precision of fractional numbers.
    sassPrecision         :: Int
    -- | Output style for the generated css code.
  , sassOutputStyle       :: Lib.SassOutputStyle
    -- | Emit comments in the generated CSS indicating the corresponding source
    --   line.
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
    --   define something with string compilation or to overload the input file
    --   path.
  , sassInputPath         :: Maybe FilePath
    -- | The output path used for source map generation.
  , sassOutputPath        :: Maybe FilePath
    -- | Paths used to load plugins by libsass.
  , sassPluginPaths       :: Maybe [FilePath]
    -- | Paths used to resolve @include.
  , sassIncludePaths      :: Maybe [FilePath]
    -- | Path to source map file. Enables source map generation and is used to
    --   create sourceMappingUrl
  , sassSourceMapFile     :: Maybe FilePath
    -- | Directly inserted in source maps.
  , sassSourceMapRoot     :: Maybe String
    -- | List of user-supplied functions that may be used in sass files.
  , sassFunctions         :: Maybe [SassFunction]
}

instance Default SassOptions where
    def = SassOptions {
        sassPrecision         = 5
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
    }

-- | Copies 'SassOptions' to native object, excluding 'sassFunctions'.
copyOptionsToNative :: SassOptions -> Ptr Lib.SassOptions -> IO ()
copyOptionsToNative opt ptr = do
    Lib.sass_option_set_precision ptr (fromIntegral $ sassPrecision opt)
    Lib.sass_option_set_output_style ptr
        (fromIntegral $ fromEnum $ sassOutputStyle opt)
    Lib.sass_option_set_source_comments ptr (sassSourceComments opt)
    Lib.sass_option_set_source_map_embed ptr (sassSourceMapEmbed opt)
    Lib.sass_option_set_source_map_contents ptr (sassSourceMapContents opt)
    Lib.sass_option_set_omit_source_map_url ptr (sassOmitSourceMapUrl opt)
    Lib.sass_option_set_is_indented_syntax_src ptr (sassIsIndentedSyntax opt)
    withCString (sassIndent opt) (Lib.sass_option_set_indent ptr)
    withCString (sassLinefeed opt) (Lib.sass_option_set_linefeed ptr)
    withOptionalCString (sassInputPath opt)
        (Lib.sass_option_set_input_path ptr)
    withOptionalCString (sassOutputPath opt)
        (Lib.sass_option_set_output_path ptr)
    withOptionalCString (concatPaths <$> sassPluginPaths opt)
        (Lib.sass_option_set_plugin_path ptr)
    withOptionalCString (concatPaths <$> sassIncludePaths opt)
        (Lib.sass_option_set_include_path ptr)
    withOptionalCString (sassSourceMapFile opt)
        (Lib.sass_option_set_source_map_file ptr)
    withOptionalCString (sassSourceMapRoot opt)
        (Lib.sass_option_set_source_map_root ptr)

-- | Copies 'sassFunctions' to native object, executes action, clears leftovers
--   (see documentation of 'makeNativeFunction') and returns action result.
withFunctions :: SassOptions -- ^ Options.
              -> Ptr Lib.SassOptions -- ^ Native options.
              -> IO a -- ^ Action.
              -> IO a -- ^ Result
withFunctions opt ptr action =
    case (sassFunctions opt) of
        Nothing -> action
        Just lst -> do
            nativeFnList <- makeNativeFunctionList lst
            Lib.sass_option_set_c_functions ptr nativeFnList
            !result <- action
            clearNativeFunctionList nativeFnList
            return result