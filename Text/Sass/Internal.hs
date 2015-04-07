module Text.Sass.Internal
  (
    copyToOptions
  ) where

import qualified Binding.Libsass     as Lib
import           Control.Applicative ((<$>))
import           Foreign
import           Foreign.C
import           Text.Sass.Utils
import           Text.Sass.Types

-- | Copies 'SassOptions' to native 'Lib.SassOptions'.
copyToOptions :: SassOptions -> Ptr Lib.SassOptions -> IO ()
copyToOptions opt ptr = do
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