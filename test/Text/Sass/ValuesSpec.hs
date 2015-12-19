{-# LANGUAGE StandaloneDeriving #-}
module Text.Sass.ValuesSpec where

import           Test.Hspec
import           Text.Sass
import           Text.Sass.Values.Utils

optsWithVal :: SassValue -> SassOptions
optsWithVal v = def {
    sassFunctions = Just [SassFunction "foo()" (\_ -> return v)]
}

testSerialize :: SassValue -> String -> Expectation
testSerialize v e =
    let opts = optsWithVal v
    in compileString "foo { prop: foo(); }" opts `shouldReturn`
        Right ("foo {\n  prop: " ++ e ++ "; }\n")

fooFunction :: SassValue -> SassValue -> IO SassValue
fooFunction e (SassList [a] _) = return $ if e == a
    then a
    else SassNull
fooFunction _ _ = error "fooFunction: invalid arguments"

optsWithArg :: SassValue -> SassOptions
optsWithArg a = def {
    sassFunctions = Just [SassFunction "foo($a)" (fooFunction a)]
}

testDeserialize :: SassValue -> String -> Expectation
testDeserialize a e =
    let opts = optsWithArg a
    in compileString ("foo { prop: foo(" ++ e ++ "); }") opts `shouldReturn`
        Right ("foo {\n  prop: " ++ e ++ "; }\n")

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should serialize boolean true" $
        testSerialize (SassBool True) "true"

    it "should serialize boolean false" $
        testSerialize (SassBool False) "false"

    it "should serialize number" $
        testSerialize (SassNumber 1 "px") "1px"

    it "should serialize color" $
        testSerialize (SassColor 1 2 3 0.5) "rgba(1, 2, 3, 0.5)"

    it "should serialize string" $
        testSerialize (SassString "abcd") "abcd"

    it "should serialize list" $
        testSerialize
            (SassList [SassNumber 1 "px", SassNumber 2 "px"]
             SassSeparatorComma)
            "1px, 2px"

    it "should serialize map" $ do
        let map = (SassMap [ (SassString "a", SassString "b")
                  , (SassNumber 1 "", SassBool True)])
        let opts = optsWithVal map
        result <- compileString "@each $key, $val in foo() { #{$key} { val: #{$val} } }" opts
        result `shouldBe` Right "a {\n  val: b; }\n\n1 {\n  val: true; }\n"

    it "should deserialize boolean true" $
        testDeserialize (SassBool True) "true"

    it "should deserialize boolean false" $
        testDeserialize (SassBool False) "false"

    it "should deserialize number" $
        testDeserialize (SassNumber 1 "px") "1px"

    it "should deserialize color" $
        testDeserialize (SassColor 1 2 3 0.5) "rgba(1, 2, 3, 0.5)"

    it "should deserialize string" $
        testDeserialize (SassString "abcd") "abcd"

    it "should deserialize list" $
        testDeserialize
            (SassList [SassNumber 1 "px", SassNumber 2 "px"] SassSeparatorSpace)
            "1px 2px"

    it "should deserialize map" $
        pendingWith "Libsass does not allow maps in CSS any more"

    it "should correctly combine two SassValues" $ do
        let val1 = SassNumber 1.0 ""
            val2 = SassNumber 2.0 ""
        combineSassValues SassAdd val1 val2 `shouldBe` SassNumber 3.0 ""
