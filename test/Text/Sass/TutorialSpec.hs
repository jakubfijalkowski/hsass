module Text.Sass.TutorialSpec where

import           Control.Concurrent.MVar
import           Test.Hspec
import           Text.Sass

max3 :: SassValue -> IO SassValue
max3 (SassList (SassNumber a _:SassNumber b _:SassNumber c _:_) _) =
    return $ SassNumber (max a $ max b c) "px"
max3 _ = return $ SassError "invalid arguments"

max3sig :: SassFunction
max3sig = SassFunction "max3($a, $b, $c)" max3

warn :: MVar String -> SassValue -> IO SassValue
warn m (SassList (SassString s:_) _) = do
    putMVar m s
    return $ SassString "warn!"
warn _ _ = return $ SassString "invalid arguments"

warnSig :: MVar String -> SassFunction
warnSig = SassFunction "@warn" . warn

header :: String -> IO [SassImport]
header src = return [makeSourceImport $ "$file: " ++ src ++ ";"]

headerSig :: SassImporter
headerSig = SassImporter 1 header

importer1 :: String -> IO [SassImport]
importer1 src = return [makeSourceImport $ "$file: " ++ src ++ "1;"]

importer2 :: String -> IO [SassImport]
importer2 src = return [makeSourceImport $ "$file: " ++ src ++ "2;"]

importerSigs :: [SassImporter]
importerSigs = [SassImporter 0.5 importer1, SassImporter 1 importer2]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "max3" $ do
        it "should return max value" $ do
            let vals = SassList [SassNumber 1 "", SassNumber 2 "",
                                 SassNumber 3 ""] SassSeparatorSpace
            SassNumber v _ <- max3 vals
            v `shouldBe` 3

        it "should return error on invalid aruments" $ do
            let vals = SassList [SassString "", SassString "",
                                 SassNumber 3 ""] SassSeparatorSpace
            SassError e <- max3 vals
            e `shouldBe` "invalid arguments"

        it "should be usable by compile" $ do
            let opts = def { sassFunctions = Just [max3sig] }
            compileString "foo { margin: max3(1px, 2px, 3px); }" opts
                `shouldReturn` Right "foo {\n  margin: 3px; }\n"

    describe "warn" $
        it "should call warn on @warn statement" $ do
            msg <- newEmptyMVar
            let opts = def { sassFunctions = Just [warnSig msg] }
            _ <- compileString "@warn \"message\";" opts :: StringResult
            tryTakeMVar msg `shouldReturn` Just "message"

    describe "Headers" $
        it "should inject header into file" $ do
            let opts = def {
                sassHeaders = Just [headerSig]
              , sassInputPath = Just "path"
            }
            compileString "foo { prop: $file; }" opts `shouldReturn`
                Right "foo {\n  prop: path; }\n"

    describe "Importers" $
        it "should inject import with higher priority" $ do
            let opts = def { sassImporters = Just importerSigs }
            compileString "@import \"file\";\nfoo { prop: $file; }" opts
                `shouldReturn` Right "foo {\n  prop: file2; }\n"
