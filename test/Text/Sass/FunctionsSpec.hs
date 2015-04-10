module Text.Sass.FunctionsSpec where

import           Test.Hspec
import           Text.Sass

fooFunction :: SassValue -> IO SassValue
fooFunction _ = return $ SassNumber 1 "px"

barFunction :: SassValue -> IO SassValue
barFunction (SassList [SassString "a"] _) = return $ SassNumber 1 "px"
barFunction _ = return $ SassError "invalid arguments"

functions :: [SassFunction]
functions =
  [ SassFunction "foo()"   fooFunction
  , SassFunction "bar($n)" barFunction
  ]

inclContent :: String
inclContent = "a {\n  margin: 1px; }\n"

altInclContent :: String
altInclContent = "b {\n  margin: 5px; }\n"

headerFunction :: String -> IO [SassImport]
headerFunction _ = return [makeSourceImport inclContent]

headers :: [SassImporter]
headers = [SassImporter 1 headerFunction]

importFunction :: String -> IO [SassImport]
importFunction "_imp" = return [makeSourceImport inclContent]
importFunction _      = return [makeSourceImport altInclContent]

importers :: [SassImporter]
importers = [SassImporter 1 importFunction]

spec :: Spec
spec = do
    it "should call simple function" $ do
        let opts = def { sassFunctions = Just functions }
        compileString "a { margin: foo(); }" opts `shouldReturn`
            Right "a {\n  margin: 1px; }\n"

    it "should correctly pass arguments to function" $ do
        let opts = def { sassFunctions = Just functions }
        compileString "a { margin: bar('a'); }" opts `shouldReturn`
            Right "a {\n  margin: 1px; }\n"

    it "should correctly inject header" $ do
        let opts = def { sassHeaders = Just headers }
        compileString "a { margin : 1px; }" opts `shouldReturn`
            Right (inclContent ++ "\na {\n  margin: 1px; }\n")

    it "should call importers" $ do
        let opts = def { sassImporters = Just importers }
        compileString "@import '_imp';" opts `shouldReturn` Right inclContent

    it "should pass import name to importers" $ do
        let opts = def { sassImporters = Just importers }
        compileString "@import 'other';" opts `shouldReturn` Right altInclContent

