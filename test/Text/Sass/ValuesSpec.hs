module Text.Sass.ValuesSpec where

import           Test.Hspec
import           Text.Sass

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

spec :: Spec
spec = do
    it "should serialize boolean true" $ do
        testSerialize (SassBool True) "true"

    it "should serialize boolean false" $ do
        testSerialize (SassBool False) "false"

    it "should serialize number" $ do
        testSerialize (SassNumber 1 "px") "1px"

    it "should serialize color" $ do
        testSerialize (SassColor 1 2 3 0.5) "rgba(1, 2, 3, 0.5)"

    it "should serialize string" $ do
        testSerialize (SassString "abcd") "abcd"

    it "should serialize list" $ do
        testSerialize
            (SassList [SassNumber 1 "px", SassNumber 2 "px"] SassSeparatorComma)
            "1px, 2px"

    it "should serialize map" $ do
        testSerialize
            (SassMap [ (SassString "a", SassString "b")
                     , (SassNumber 1 "", SassBool True)])
            "(a: b, 1: true)"

    it "should deserialize boolean true" $ do
        testDeserialize (SassBool True) "true"

    it "should deserialize boolean false" $ do
        testDeserialize (SassBool False) "false"

    it "should deserialize number" $ do
        testDeserialize (SassNumber 1 "px") "1px"

    it "should deserialize color" $ do
        testDeserialize (SassColor 1 2 3 0.5) "rgba(1, 2, 3, 0.5)"

    it "should deserialize string" $ do
        testDeserialize (SassString "abcd") "abcd"

    it "should deserialize list" $ do
        testDeserialize
            (SassList [SassNumber 1 "px", SassNumber 2 "px"] SassSeparatorSpace)
            "1px 2px"

    it "should deserialize map" $ do
        testDeserialize
            (SassMap [ (SassString "a", SassString "b")
                     , (SassNumber 1 "", SassBool True)])
            "(a: b, 1: true)"