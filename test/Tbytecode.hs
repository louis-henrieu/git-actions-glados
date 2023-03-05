module Tbytecode (
    testBytecode
) where

-- Test import
import Test.Hspec

-- Project import
import Bytecode

testBytecode :: Spec
testBytecode = describe "\nTest all functions of Bytecode file" $ do
    testCreateByteCode

testCreateByteCode :: Spec
testCreateByteCode = describe "-- createByteCode --" $ do
    it "basic bytecode" $ do
        let test = createByteCode
        let rest = show test
        rest `shouldBe` "<function>"