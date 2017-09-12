-- | Copying 'SassOptions' into native context. This module is internal and
-- should not be considered stable.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
module Text.Sass.Options.Internal
  (
    copyOptionsToNative
  , withFunctions
  ) where

import qualified Bindings.Libsass             as Lib
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative          ((<$>))
#endif
import           Control.Monad                ((>=>))
import           Foreign
import           Foreign.C
import           Text.Sass.Functions
import           Text.Sass.Functions.Internal
import           Text.Sass.Options
import           Text.Sass.Utils

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
    maybe (return ())
        (makeNativeImporterList . fmap fromHeader
         >=> Lib.sass_option_set_c_headers ptr)
        (sassHeaders opt)
    maybe (return ())
        (makeNativeImporterList >=> Lib.sass_option_set_c_importers ptr)
        (sassImporters opt)
    where
      fromHeader (SassHeader p f) = SassImporter p (\filename _ -> f filename)


-- | Copies 'sassFunctions' to native object, executes action, clears leftovers
-- (see documentation of 'makeNativeFunction') and returns action result.
withFunctions :: SassOptions -- ^ Options.
              -> Ptr Lib.SassOptions -- ^ Native options.
              -> IO a -- ^ Action.
              -> IO a -- ^ Result
withFunctions opt ptr action =
    case sassFunctions opt of
        Nothing -> action
        Just lst -> do
            nativeFnList <- makeNativeFunctionList lst
            Lib.sass_option_set_c_functions ptr nativeFnList
            !result <- action
            clearNativeFunctionList nativeFnList
            return result
