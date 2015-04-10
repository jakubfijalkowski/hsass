module Text.Sass.Functions
  (
    SassFunctionType
  , SassFunction (..)
  ) where

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
