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
    testEraseDoubles

testGetValueEnv :: Spec
testGetValueEnv = describe "Test getValueEnv function" $ do
    it "Test getValueEnv with empty env" $ do
        let test = getValueEnv [] "a"
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'a' not found\""
    it "Test getValueEnv with env with one value" $ do
        let test = getValueEnv [("a", IntegerAst 1)] "a"
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test getValueEnv with env with two values" $ do
        let test = getValueEnv [("a", IntegerAst 1), ("b", IntegerAst 2)] "b"
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 2)"

testEraseDoubles :: Spec
testEraseDoubles = describe "Test eraseDoubles function" $ do
    it "Test eraseDoubles with empty env" $ do
        let test = eraseDoubles [] []
        let rest = show test
        rest `shouldBe` "[]"
    it "Test eraseDoubles with env with one value" $ do
        let test = eraseDoubles [("a", IntegerAst 1)] []
        let rest = show test
        rest `shouldBe` "[(\"a\",IntegerAst 1)]"
    it "Test eraseDoubles with env with two values" $ do
        let test = eraseDoubles [("a", IntegerAst 1), ("b", IntegerAst 2)] []
        let rest = show test
        rest `shouldBe` "[(\"b\",IntegerAst 2),(\"a\",IntegerAst 1)]"
    it "Test eraseDoubles with env with two values and one double" $ do
        let test = eraseDoubles [("a", IntegerAst 1), ("b", IntegerAst 2)] [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "[(\"b\",IntegerAst 2),(\"a\",IntegerAst 1)]"
    it "Test eraseDoubles with env with two values and two doubles" $ do
        let test = eraseDoubles [("a", IntegerAst 1), ("b", IntegerAst 2)] [("a", IntegerAst 1), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "[(\"a\",IntegerAst 1),(\"b\",IntegerAst 2)]"
    it "Test eraseDoubles with env with two values and two doubles and one value" $ do
        let test = eraseDoubles [("a", IntegerAst 1), ("b", IntegerAst 2)] [("a", IntegerAst 1), ("b", IntegerAst 2), ("c", IntegerAst 3)]
        let rest = show test
        rest `shouldBe` "[(\"a\",IntegerAst 1),(\"b\",IntegerAst 2),(\"c\",IntegerAst 3)]"