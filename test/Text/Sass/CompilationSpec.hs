module Text.Sass.CompilationSpec where

import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Text.Sass

spec :: Spec
spec = do
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

