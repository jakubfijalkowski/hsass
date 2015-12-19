-- | Provides API for managing values that may be extracter from or injected to
-- sass source file.
module Text.Sass.Values
  (
    SassMapEntry
  , SassValue (..)
  , Lib.SassSeparator (..)
  ) where

import qualified Bindings.Libsass as Lib

-- | Entry of 'SassMap'.
type SassMapEntry = (SassValue, SassValue)

-- | Represents value used by libsass.
data SassValue = SassBool Bool -- ^ Boolean value.
               | SassNumber Double String -- ^ Number (value and unit).
               | SassColor {
                    sassColorR :: Double,
                    sassColorG :: Double,
                    sassColorB :: Double,
                    sassColorA :: Double
                 } -- ^ RGBA color.
                | SassString String -- ^ String
                | SassList [SassValue] Lib.SassSeparator
                  -- ^ List of 'SassValue's.
                | SassMap [SassMapEntry] -- ^ Map.
                | SassNull -- ^ Null value.
                | SassWarning String -- ^ Warning with message.
                | SassError String -- ^ Error with message.
                deriving (Eq, Show)
