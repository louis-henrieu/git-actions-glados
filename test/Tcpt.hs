module Tcpt
    (
        testCpt
    ) where

-- Test import
import Test.Hspec

-- Project import
import Cpt

testCpt :: Spec
testCpt = describe "\nTest all functions of Cpt file" $ do
    testStruct
    testPrintTree

testStruct :: Spec
testStruct = describe "-- data Cpt --" $ do
    let test = Symbol "apit"
    let rest = show test
    it "\tSymbol" $ do
        rest `shouldBe` "Symbol \"apit\""

testPrintTree :: Spec
testPrintTree = describe "-- printTree --" $ do
        it "\tSymbol" $ do
            printTree (Symbol "apit") `shouldBe` "a Symbol 'apit'"
        it "\tNumber" $ do
            printTree (Number 5) `shouldBe` "a Number '5'"
        it "\tNumberFloat" $ do
            printTree (NumberFloat 10.09) `shouldBe` "a Number '10.09'"
        it "\tList" $ do
            printTree (List [Symbol "+", Number 10, Number 3]) `shouldBe` "a List with a Symbol '+' a Number '10' a Number '3'"

