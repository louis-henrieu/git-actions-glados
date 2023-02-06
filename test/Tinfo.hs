module Tinfo
    (
        testInfo
    ) where

-- Test import
import Test.Hspec

-- Project import
import Info

testInfo :: Spec
testInfo = describe "\nTest all functions of Info file" $ do
    testGetValueEnv

testGetValueEnv :: Spec
testGetValueEnv = describe "Test getValueEnv function" $ do
    it "Test getValueEnv with empty env" $ do
        let test = getValueEnv [] "a"
        let rest = show test
        rest `shouldBe` "Left \"Symbole : 'a' not found\""
    it "Test getValueEnv with env with one value" $ do
        let test = getValueEnv [("a", IntegerAst 1)] "a"
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test getValueEnv with env with two values" $ do
        let test = getValueEnv [("a", IntegerAst 1), ("b", IntegerAst 2)] "b"
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 2)"