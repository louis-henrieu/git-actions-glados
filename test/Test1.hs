module Test1
    (
        test1
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Temp

test1 :: Spec
test1 = describe "Test1" $ do
    testBasic

testBasic :: Spec
testBasic = describe "Basic" $ do
    describe "Basic test" $ do
        it "test basic return for 34" $ do
                Temp.nexti 'a' `shouldBe` 'd'
--        it "returns next Int for 42" $ nexti 42 `shouldBe` 43
--        it "returns next Int for negative" $ nexti (-5) `shouldBe` (-4)
--        it "returns next Int for 0" $ nexti 0 `shouldBe` 1