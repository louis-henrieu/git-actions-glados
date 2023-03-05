module Tstack (
    testStack
) where

-- Test import
import Test.Hspec

-- Project import
import Stack
import Info

testStack :: Spec
testStack = describe "\nTest all functions of Stack file" $ do
    it "basic parseCpt" $ do
        let rest = show "test"
        rest `shouldBe` "\"test\""
    --testInitStack

--testInitStack :: Spec
--testInitStack = describe "-- initStack --" $ do
--    it "basic initStack" $ do
--        shinitStack `shouldBe` Stack { fast = [], global = [], constValue = [], bytecode = [], end = False, numFunctions = 0, codeLine = 1}