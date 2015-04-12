module Text.Sass.TestingUtils where

import           Test.Hspec

-- | @a `returnShouldSatisfy` p@ asserts that the action @a@ returns value that
--   satisfies predicate @p@.
returnShouldSatisfy :: (Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
v `returnShouldSatisfy` p = v >>= (`shouldSatisfy` p)
