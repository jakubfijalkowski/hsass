{-# LANGUAGE BangPatterns #-}
-- | Provides utility functions for working with 'SassValues'
module Text.Sass.Values.Utils
  (
    combineSassValues
  , Lib.SassOp (..)
  ) where

import qualified Bindings.Libsass as Lib

import System.IO.Unsafe

import Text.Sass.Values
import Text.Sass.Values.Internal

-- | Combines two 'SassValue's using specified operator.
-- |
-- | Uses 'Lib.sass_value_op'.
combineSassValues :: Lib.SassOp -- ^ Operator.
                  -> SassValue  -- ^ First value.
                  -> SassValue  -- ^ Second value.
                  -> SassValue  -- ^ Resulting value.
combineSassValues op val1 val2 = unsafePerformIO $ do
    native1 <- toNativeValue val1
    native2 <- toNativeValue val2
    combined <- Lib.sass_value_op (fromIntegral $ fromEnum op) native1 native2
    !result <- fromNativeValue combined
    deleteNativeValue native1
    deleteNativeValue native2
    deleteNativeValue combined
    return result
