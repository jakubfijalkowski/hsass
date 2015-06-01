module Text.Sass.CompilationSpec where

import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Text.Sass

import           Data.Either            (isLeft, isRight)
import           Data.Maybe             (isJust)
import           Text.Sass.TestingUtils

main :: IO ()
main = hspec spec

importerFunc :: String -> IO [SassImport]
importerFunc _ = return [makeSourceImport "a { margin: 1px; }"]

importers :: [SassImporter]
importers = [SassImporter 1 importerFunc]

extendedResultSpec, compilationSpec, errorReportingSpec, spec :: Spec
spec = do
    describe "Compilation" compilationSpec
    describe "Extended compilation" extendedResultSpec
    describe "Error reporting" errorReportingSpec

compilationSpec = do
    it "should compile simple source" $
        compileString "foo { margin: 21px * 2; }" def `shouldReturn`
            Right "foo {\n  margin: 42px; }\n"

    it "should respect options" $ do
        let opts = def { sassOutputStyle = SassStyleCompressed }
        compileString "foo { margin: 21px * 2; }" opts `shouldReturn`
            Right "foo{margin:42px}\n"

    it "should compile file" $
        withSystemTempFile "styles.sass" $ \p h -> do
            hPutStr h "foo { margin: 21px * 2; }"
            hClose h
            compileFile p def `shouldReturn` Right "foo {\n  margin: 42px; }\n"

    it "compile file should respect options" $ do
        let opts = def { sassOutputStyle = SassStyleCompressed }
        withSystemTempFile "styles.sass" $ \p h -> do
            hPutStr h "foo { margin: 21px * 2; }"
            hClose h
            compileFile p opts `shouldReturn` Right "foo{margin:42px}\n"

extendedResultSpec = do
    it "should compile simple source" $ do
        res <- compileString "foo { margin: 21px * 2; }" def
        res `shouldSatisfy` isRight
        let Right res' = res
        resultString res' `shouldBe` "foo {\n  margin: 42px; }\n"

    it "should report correct includes when available" $ do
        let opts = def { sassImporters = Just importers }
        Right res <- compileString "@import '_abc';" opts
        resultIncludes res `shouldReturn` [ "_abc" ]

    it "should report no includes when unavailable" $ do
        Right res <- compileString "foo { margin: 1px; }" def
        resultIncludes res `shouldReturn` [ ]

    it "should retrieve source map if available" $ do
        let opts = def { sassSourceMapFile = Just "abc.css" }
        Right res <- compileString "foo { margin: 1px; }" opts
        m <- resultSourcemap res
        m `shouldSatisfy` isJust
        let Just m' = m
        m' `shouldSatisfy` (not . null)

    it "should return Nothing is source map if not available" $ do
        Right res <- compileString "foo { margin: 1px; }" def
        resultSourcemap res `shouldReturn` Nothing

errorReportingSpec = do
    it "string compilation should report error on invalid code" $
        (compileString "inv mark" def :: StringResult)
            `returnShouldSatisfy` isLeft

    it "file compilation should report error on invalid code" $
        withSystemTempFile "styles.sass" $ \p h -> do
            hPutStr h "!@# !@##  ## #"
            hClose h
            (compileFile p def :: StringResult) `returnShouldSatisfy` isLeft

    it "should contain line" $ do
        (Left r) <- compileString "invalid mark" def :: StringResult
        errorLine r `shouldReturn` 1

    it "should contain column" $ do
        (Left r) <- compileString "body { !! }" def :: StringResult
        errorColumn r `shouldReturn` 6

    it "should contain description" $ do
        (Left r) <- compileString "body { !! }" def :: StringResult
        errorText r `returnShouldSatisfy` (not . null)

    it "should contain message" $ do
        (Left r) <- compileString "body { !! }" def :: StringResult
        errorMessage r `returnShouldSatisfy` (not . null)

    it "should contain Json description" $ do
        (Left r) <- compileString "body { !! }" def :: StringResult
        errorJson r `returnShouldSatisfy` (not . null)

    it "should contain file path" $ do
        (Left r) <- compileString "body { !! }" def :: StringResult
        errorFile r `returnShouldSatisfy` (not . null)
