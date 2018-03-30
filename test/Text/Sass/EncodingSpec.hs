module Text.Sass.EncodingSpec where

import           Control.Concurrent.MVar
import           Data.List  (isSuffixOf)
import           Test.Hspec
import           Text.Sass
import qualified Text.Sass.Values.Internal as VI

opts :: SassOptions
opts = def { sassStripEncodingInfo = True }

testString :: String
testString =  "Zażółcić gęślą jaźń"

testInput :: String
testInput = "a { content: '" ++ testString ++ "'; }" 

expectedResult :: Either a String
expectedResult = Right $ "a {\n  content: '" ++ testString ++ "'; }\n"

main :: IO ()
main = hspec spec

testValue :: SassValue -> Expectation
testValue v =
    (VI.toNativeValue v >>= VI.fromNativeValue) `shouldReturn` v

storeFunc :: MVar String -> SassValue -> IO SassValue
storeFunc m (SassList [SassString s] _) = do
    putMVar m s
    return $ SassString s
storeFunc _ _ = fail "Unknown value"

storeFuncSig :: MVar String -> SassFunction
storeFuncSig = SassFunction "store($s)" . storeFunc

returnFunc :: SassValue -> IO SassValue
returnFunc _ = return $ SassString $ "'" ++ testString ++ "'"

returnFuncSig :: SassFunction
returnFuncSig = SassFunction "ret()" returnFunc

nonUTFNameSig :: SassFunction
nonUTFNameSig = SassFunction "zażółcić()" returnFunc

importFunction :: String -> String -> IO [SassImport]
importFunction "src"  _ = return [makeSourceImport testInput]
importFunction "path" _ = return [makePathImport "test/data/pl-zażółcić.scss" "."]
importFunction _ _ = fail "Unknown import"

importers :: [SassImporter]
importers = [SassImporter 1 importFunction]

spec :: Spec
spec = do
    it "should preserve UTF-8 encoding when compiling Strings" $
        compileString testInput opts `shouldReturn` expectedResult

    it "should preserve UTF-8 encoding when compiling files" $
        compileFile "test/data/pl.scss" opts `shouldReturn` expectedResult

    it "should correctly encode UTF-8 chars in source paths" $
        compileFile "test/data/pl-zażółcić.scss" opts
            `shouldReturn` expectedResult

    it "should correctly encode UTF-8 chars in included files path" $ do
        Right res <- compileFile "test/data/imports.scss" opts :: ExtendedResult
        includes <- resultIncludes res
        includes `shouldSatisfy` any (isSuffixOf "test/data/pl-zażółcić.scss")

    it "should correctly encode/decode UTF-8 SassStrings" $
        testValue $ SassString testString

    it "should correctly encode/decode UTF-8 units in SassNumbers" $
        testValue $ SassNumber 1 testString

    it "should correctly encode/decode UTF-8 SassWarnings" $
        testValue $ SassWarning testString

    it "should correctly encode/decode UTF-8 SassErrors" $
        testValue $ SassError testString

    it "should correctly encode/decode list of UTF-8 SassStrings" $
        testValue $ SassList [SassString "Zażółcić", SassString "gęślą"] SassSeparatorComma

    it "should correctly UTF-8 decode function arguments" $ do
        arg <- newEmptyMVar
        let fopts = opts { sassFunctions = Just [ storeFuncSig arg ] }
        _ <- compileString ("a {foo: store('" ++ testString ++ "');}") fopts :: StringResult
        tryTakeMVar arg `shouldReturn` Just testString

    it "should correctly UTF-8 encode function return value" $ do
        let fopts = opts { sassFunctions = Just [ returnFuncSig ] }
        compileString "a {content: ret(); }" fopts `shouldReturn` expectedResult

    it "should correctly UTF-8 encode function names" $ do
        let fopts = opts { sassFunctions = Just [ nonUTFNameSig ] }
        compileString "a {content: zażółcić(); }" fopts
            `shouldReturn` expectedResult

    it "should correctly encode UTF-8 string from importer" $
        let iopts = opts { sassImporters = Just importers }
         in compileString "@import 'src';" iopts `shouldReturn` expectedResult

    it "should correctly encode UTF-8 path from importer" $
        let iopts = opts { sassImporters = Just importers }
         in compileString "@import 'path';" iopts `shouldReturn` expectedResult
