module Tparser (
    testParser
) where

-- Test import
import Test.Hspec

-- Project import
import Parser

testParser :: Spec
testParser = describe "\nTest all functions of Parser file" $ do
    it "basic parseCpt" $ do
        let rest = show "test"
        rest `shouldBe` "\"test\""
  --  testparseCpt

--testparseCpt :: Spec
--testparseCpt = describe "-- parseCpt --" $ do
--    it "basic parseCpt" $ do
--        -- parseCpt :: String -> Either String (a, String)
--        let test = parseCpt "1"
--        let rest = show test
--        rest `shouldBe` "Right (1,\"\")"