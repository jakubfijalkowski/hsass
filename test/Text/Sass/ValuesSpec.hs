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

    it "should serialize map" $
        testSerialize
            (SassMap [ (SassString "a", SassString "b")
                     , (SassNumber 1 "", SassBool True)])
            "(a: b, 1: true)"

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
        testDeserialize
            (SassMap [ (SassString "a", SassString "b")
                     , (SassNumber 1 "", SassBool True)])
            "(a: b, 1: true)"
