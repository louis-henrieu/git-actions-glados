module Tlib (
    testLib
) where

-- Test import
import Test.Hspec

-- Project import
import Lib

testLib :: Spec
testLib = describe "\nTest all functions of Lib file" $ do
    testSomeFuncGetLine

testSomeFuncGetLine :: Spec
testSomeFuncGetLine = describe "-- someFuncGetLine --" $ do
    it "basic getLine" $ do
        let test = someFuncGetLine
        let rest = show test
        rest `shouldBe` "<function>"