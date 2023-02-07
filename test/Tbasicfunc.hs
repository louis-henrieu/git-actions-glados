module Tbasicfunc (
        testBasicFunc
    ) where

-- Test import
import Test.Hspec

-- Project import
import BasicFunc
import Info

testBasicFunc :: Spec
testBasicFunc = describe "\nTest all functions of BasicFunc file" $ do
    testCheckFloatInt
    testPreAdd
    testPreSub
    testPreMul
    testPreDiv
    testPreMod
    testFact

testCheckFloatInt :: Spec
testCheckFloatInt = describe "Test checkFloatInt function" $ do
    it "Test checkFloatInt with empty list" $ do
        let test = checkFloatInt [] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [FloatAst 1.0] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, FloatAst 2.0] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, FloatAst 2.0] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe`"Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c"), ("c", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c"), ("c", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c"), ("c", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c"), ("c", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""

testPreAdd :: Spec
testPreAdd = describe "Test preAdd function" $ do
    it "Test preAdd with empty list" $ do
        let test = preAdd [] []
        let rest = show test
        rest `shouldBe` "Left \"Add function needs at least two arguments\""
    it "Test preAdd with one element" $ do
        let test = preAdd [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preAdd with two elements" $ do
        let test = preAdd [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 3)"
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 3)"
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""

testPreSub :: Spec
testPreSub = describe "Test preSub function" $ do
    it "Test preSub with empty list" $ do
        let test = preSub [] []
        let rest = show test
        rest `shouldBe` "Left \"Sub function needs at least two arguments\""
    it "Test preSub with one element" $ do
        let test = preSub [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preSub with two elements" $ do
        let test = preSub [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst (-1))"
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "-"] [("-", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst (-1))"
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""

testPreMul :: Spec
testPreMul = describe "Test preMul function" $ do
    it "Test preMul with empty list" $ do
        let test = preMul [] []
        let rest = show test
        rest `shouldBe` "Left \"Mul function needs at least two arguments\""
    it "Test preMul with one element" $ do
        let test = preMul [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preMul with two elements" $ do
        let test = preMul [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 2)"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 2)"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    -- float
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst 1.0, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 2.0)"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""

testPreDiv :: Spec
testPreDiv = describe "Test preDiv function" $ do
    it "Test preDiv with empty list" $ do
        let test = preDiv [] []
        let rest = show test
        rest `shouldBe` "Left \"Div function needs at least two arguments\""
    --it "Test preDiv with one element" $ do
    --    let test = preDiv [IntegerAst 1] []
    --    let rest = show test
    --    rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preDiv with two elements" $ do
        let test = preDiv [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 0)"
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 0)"
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The arg 'x' is not a number\""
    it "Test preDiv with too many arguments" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a", IntegerAst 2] [("a", SymbolAst "b"), ("b", FloatAst 2.0), ("c", FloatAst 2.0)]
        let rest = show test  
        rest `shouldBe` "Left \"Div function only needs two arguments\"" 

testPreMod :: Spec
testPreMod = describe "Test preMod function" $ do
    it "Test preMod with empty list" $ do
        let test = preMod [] []
        let rest = show test
        rest `shouldBe` "Left \"Mod function needs at least two arguments\""
    --it "Test preMod with one element" $ do
    --    let test = preMod [IntegerAst 1] []
    --    let rest = show test
    --    rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preMod with two elements" $ do
        let test = preMod [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test preMod only needs two arguments" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a", IntegerAst 2] [("a", SymbolAst "b"), ("b", FloatAst 2.0), ("c", FloatAst 2.0)]
        let rest = show test  
        rest `shouldBe` "Left \"Mod function only needs two arguments\""      

testFact :: Spec
testFact = describe "Test fact function" $ do
    it "Test fact with 0" $ do
        let test = fact 0
        let rest = show test
        rest `shouldBe` "IntegerAst 1"
    it "Test fact with 1" $ do
        let test = fact 1
        let rest = show test
        rest `shouldBe` "IntegerAst 1"
    it "Test fact with 2" $ do
        let test = fact 2
        let rest = show test
        rest `shouldBe` "IntegerAst 2"
    it "Test fact with 13" $ do
        let test = fact 13
        let rest = show test
        rest `shouldBe` "IntegerAst 6227020800"