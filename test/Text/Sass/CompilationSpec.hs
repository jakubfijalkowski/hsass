module Text.Sass.CompilationSpec where

import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Text.Sass

import           Data.Either            (isLeft)
import           Text.Sass.TestingUtils

main :: IO ()
main = hspec spec

compilationSpec, errorReportingSpec, spec :: Spec
spec = do
    describe "Compilation" compilationSpec
    describe "Error reporting" errorReportingSpec

compilationSpec = do
    it "should compile simple source" $ do
        compileString "foo { margin: 21px * 2; }" def `shouldReturn`
            Right "foo {\n  margin: 42px; }\n"

    it "should respect options" $ do
        let opts = def { sassOutputStyle = SassStyleCompressed }
        compileString "foo { margin: 21px * 2; }" opts `shouldReturn`
            Right "foo{margin:42px}\n"

    it "should compile file" $ do
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

errorReportingSpec = do
    it "string compilation should report error on invalid code" $ do
        compileString "inv mark" def `returnShouldSatisfy` isLeft

    it "file compilation should report error on invalid code" $ do
        withSystemTempFile "styles.sass" $ \p h -> do
            hPutStr h "!@# !@##  ## #"
            hClose h
            compileFile p def `returnShouldSatisfy` isLeft

    it "should contain line" $ do
        (Left r) <- compileString "invalid mark" def
        errorLine r `shouldReturn` 1

    it "should contain column" $ do
        (Left r) <- compileString "body { !! }" def
        errorColumn r `shouldReturn` 6

    it "should contain description" $ do
        (Left r) <- compileString "body { !! }" def
        errorText r `returnShouldSatisfy` (not . null)

    it "should contain message" $ do
        (Left r) <- compileString "body { !! }" def
        errorMessage r `returnShouldSatisfy` (not . null)

    it "should contain Json description" $ do
        (Left r) <- compileString "body { !! }" def
        errorJson r `returnShouldSatisfy` (not . null)

    it "should contain file path" $ do
        (Left r) <- compileString "body { !! }" def
        errorFile r `returnShouldSatisfy` (not . null)
