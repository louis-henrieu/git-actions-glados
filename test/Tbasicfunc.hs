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
    testCheckZero
    testPreDiv
    testPreMod
    testPreSup
    testPreInf
    -- testFact

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
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 1)]\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 1.0)]\""
    it "Test checkFloatInt with one element" $ do
        let test = checkFloatInt [SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
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
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe`"Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 2.0)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 2.0)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"c\\\" \\n env is [(\\\"a\\\",IntegerAst 1),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c"), ("c", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"c\\\" \\n env is [(\\\"a\\\",IntegerAst 1),(\\\"b\\\",SymbolAst \\\"c\\\"),(\\\"c\\\",IntegerAst 2)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c"), ("c", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"c\\\" \\n env is [(\\\"a\\\",IntegerAst 1),(\\\"b\\\",SymbolAst \\\"c\\\"),(\\\"c\\\",FloatAst 2.0)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"c\\\" \\n env is [(\\\"a\\\",FloatAst 1.0),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c"), ("c", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"c\\\" \\n env is [(\\\"a\\\",FloatAst 1.0),(\\\"b\\\",SymbolAst \\\"c\\\"),(\\\"c\\\",IntegerAst 2)]\""
    it "Test checkFloatInt with two elements" $ do
        let test = checkFloatInt [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c"), ("c", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"c\\\" \\n env is [(\\\"a\\\",FloatAst 1.0),(\\\"b\\\",SymbolAst \\\"c\\\"),(\\\"c\\\",FloatAst 2.0)]\""

testPreAdd :: Spec
testPreAdd = describe "Test preAdd function" $ do
    it "Test preAdd with empty list" $ do
        let test = preAdd [] []
        let rest = show test
        rest `shouldBe` "Left \"Add function needs at least two arguments\""
    it "Test preAdd with one element" $ do
        let test = preAdd [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preAdd with two elements int" $ do
        let test = preAdd [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 3.0)"
    it "Test preAdd with two elements float" $ do
        let test = preAdd [FloatAst 10.51, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 11.52)"
    it "Test preAdd with two elements and one symbol int" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preAdd with two elements and one symbol float" $ do
        let test = preAdd [FloatAst 1.01, SymbolAst "a"] [("a", FloatAst 2.99)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.01)"
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [FloatAst (-1.01), SymbolAst "a"] [("a", FloatAst 0.01)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst (-1.01))"
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 2.0)]\""
    it "Test preAdd with two elements and one symbol" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
    it "Test symbol does not exist" $ do
        let test = preAdd [IntegerAst 1, SymbolAst "a"] [("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'a' not found\""
    it "Test least one symbol does not exist" $ do
        let test = preAdd [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"

testPreSub :: Spec
testPreSub = describe "Test preSub function" $ do
    it "Test preSub with empty list" $ do
        let test = preSub [] []
        let rest = show test
        rest `shouldBe` "Left \"Sub function needs at least two arguments\""
    it "Test preSub with one element" $ do
        let test = preSub [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preSub with two elements" $ do
        let test = preSub [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst (-1.0))"
    it "Test preSub with two elements float" $ do
        let test = preSub [FloatAst 10.51, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 9.5)"
    it "Test preSub with two elements and one symbol int" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preSub with two elements and one symbol float" $ do
        let test = preSub [FloatAst 1.01, SymbolAst "a"] [("a", FloatAst 2.99)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.01)"
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [FloatAst (-1.01), SymbolAst "a"] [("a", FloatAst 0.01)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst (-1.01))"
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 2.0)]\""
    it "Test preSub with two elements and one symbol" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
    it "Test symbol does not exist" $ do
        let test = preSub [IntegerAst 1, SymbolAst "a"] [("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'a' not found\""

testPreMul :: Spec
testPreMul = describe "Test preMul function" $ do
    it "Test preMul with empty list" $ do
        let test = preMul [] []
        let rest = show test
        rest `shouldBe` "Left \"Mul function needs at least two arguments\""
    it "Test preMul with one element" $ do
        let test = preMul [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preMul with two elements" $ do
        let test = preMul [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 2.0)"
    it "Test preMul with two elements float" $ do
        let test = preMul [FloatAst 10.51, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 10.6151)"
    it "Test preMul with two elements and one symbol int" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preMul with two elements and one symbol float" $ do
        let test = preMul [FloatAst 1.01, SymbolAst "a"] [("a", FloatAst 2.99)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.01)"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst (-1.01), SymbolAst "a"] [("a", FloatAst 0.01)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst (-1.01))"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 2.0)]\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
    -- float
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst 1.0, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test preMul with two elements and one symbol" $ do
        let test = preMul [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test symbol does not exist" $ do
        let test = preMul [IntegerAst 1, SymbolAst "a"] [("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'a' not found\""


testCheckZero :: Spec
testCheckZero = describe "Test checkZero function" $ do
    it "Test checkZero with zero" $ do
        let test = checkZero (IntegerAst 0) [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"Second argument is not allowed to be a 0\""
    it "Test checkZero with positive num" $ do
        let test = checkZero (IntegerAst 1) [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test checkZero with negative num" $ do
        let test = checkZero (IntegerAst (-1)) [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst (-1))"
    it "Test checkZero with float zero" $ do
        let test = checkZero (FloatAst 0.0) [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"Second argument is not allowed to be a 0\""
    it "Test checkZero with float positive num" $ do
        let test = checkZero (FloatAst 1.0) [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test checkZero with float negative num" $ do
        let test = checkZero (FloatAst (-1.0)) [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst (-1.0))"
    it "Test checkZero with symbol" $ do
        let test = checkZero (SymbolAst "a") [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test checkZero with symbol 0 int" $ do
        let test = checkZero (SymbolAst "a") [("a", IntegerAst 0)]
        let rest = show test
        rest `shouldBe` "Left \"Second argument is not allowed to be a 0\"" 
    it "Test checkZero with symbol float pos" $ do
        let test = checkZero (SymbolAst "a") [("a", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test checkZero with symbol float 0" $ do
        let test = checkZero (SymbolAst "a") [("a", FloatAst 0)]
        let rest = show test
        rest `shouldBe` "Left \"Second argument is not allowed to be a 0\""
    it "Test checkZero with symbol" $ do
        let test = checkZero (SymbolAst "a") [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test checkZero with symbol" $ do
        let test = checkZero (SymbolAst "a") [("a", SymbolAst "b"), ("b", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""

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
        rest `shouldBe` "Right (FloatAst 0.5)"
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\")]\""
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",IntegerAst 2)]\""
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",FloatAst 2.0)]\""
    it "Test preDiv with two elements and one symbol" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"Not a numberSymbolAst \\\"b\\\" \\n env is [(\\\"a\\\",SymbolAst \\\"b\\\"),(\\\"b\\\",SymbolAst \\\"c\\\")]\""
    it "Test preDiv with too many arguments" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a", IntegerAst 2] [("a", SymbolAst "b"), ("b", FloatAst 2.0), ("c", FloatAst 2.0)]
        let rest = show test  
        rest `shouldBe` "Left \"Div function only needs two arguments\""
    it "Test symbol does not exist" $ do
        let test = preDiv [IntegerAst 1, SymbolAst "a"] [("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe`"Left \"Symbol 'a' not found\""
 

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
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements float" $ do
        let test = preMod [FloatAst 10.51, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements and one symbol int" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements and one symbol float" $ do
        let test = preMod [FloatAst 1.01, SymbolAst "a"] [("a", FloatAst 2.99)]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [FloatAst (-1.01), SymbolAst "a"] [("a", FloatAst 0.01)]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod with two elements and one symbol" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""
    it "Test preMod only needs two arguments" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a", IntegerAst 2] [("a", SymbolAst "b"), ("b", FloatAst 2.0), ("c", FloatAst 2.0)]
        let rest = show test  
        rest `shouldBe` "Left \"Mod function only needs two arguments\""
    it "Test symbol does not exist" $ do
        let test = preMod [IntegerAst 1, SymbolAst "a"] [("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"There is at least one parameter that isn't an integer\""

testPreSup :: Spec
testPreSup = describe "Test preSup function" $ do
    it "Test preSup with empty list" $ do
        let test = preSup [] []
        let rest = show test
        rest `shouldBe` "Left \"> operator needs at least two arguments\""
    --it "Test preSup with one element" $ do
    --    let test = preSup [IntegerAst 1] []
    --    let rest = show test
    --    rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preSup with two elements" $ do
        let test = preSup [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"
    it "Test preSup with two elements float" $ do
        let test = preSup [FloatAst 10.51, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "Test preSup with two elements and one symbol int" $ do
        let test = preSup [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a number\""
    it "Test preSup with two elements and one symbol float" $ do
        let test = preSup [FloatAst 1.01, SymbolAst "a"] [("a", FloatAst 2.99)]
        let rest = show test
        rest `shouldBe` "Left \"Not a number\""
    it "Test preSup with two elements and one symbol" $ do
        let test = preSup [FloatAst (-1.01), SymbolAst "a"] [("a", FloatAst 0.01)]
        let rest = show test
        rest `shouldBe` "Left \"Not a number\""
    it "Test preSup with two elements and one symbol" $ do
        let test = preSup [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Not a number\""
    it "Test preSup with two elements and one symbol" $ do
        let test = preSup [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Not a number\""
    it "Test preSup with two elements and one symbol" $ do
        let test = preSup [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Not a number\""
    it "Test with only one number" $ do
        let test = preSup [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Left \"> operator needs at least two arguments\""
    it "Test with two elements x < y" $ do
        let test = preSup [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"
    it "Test with two elements x > y" $ do
        let test = preSup [IntegerAst 2, IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "Test with two float x < y" $ do
        let test = preSup [FloatAst 1.01, FloatAst 2.01] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"
    it "Test with two float x > y" $ do
        let test = preSup [FloatAst 2.01, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"

testPreInf :: Spec
testPreInf = describe "Test preInf function" $ do
    it "Test preInf with empty list" $ do
        let test = preInf [] []
        let rest = show test
        rest `shouldBe` "Left \"< operator needs at least two arguments\""
    --it "Test preInf with one element" $ do
    --    let test = preInf [IntegerAst 1] []
    --    let rest = show test
    --    rest `shouldBe` "Right (IntegerAst 1)"
    it "Test preInf with two elements x < y" $ do
        let test = preInf [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe`"Right (SymbolAst \"#t\")"
    it "Test preInf with two elements x > y" $ do
        let test = preInf [IntegerAst 2, IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"
    it "Test preInf with two elements float x < y" $ do
        let test = preInf [FloatAst 10.51, FloatAst 1.01] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"
    it "Test preInf with two elements float x > y" $ do
        let test = preInf [FloatAst 1.01, FloatAst 10.51] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "Test with only one number" $ do
        let test = preInf [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Left \"< operator needs at least two arguments\""
    --it "Test with two symbols" $ do
    --    let test = preInf [SymbolAst "a", SymbolAst "b"] []
    --    let rest = show test
    --    rest `shouldBe` "Right (SymbolAst \"#t\")"

-- testFact :: Spec
-- testFact = describe "Test fact function" $ do
--     it "Test fact with 0" $ do
--         let test = fact 0
--         let rest = show test
--         rest `shouldBe` "IntegerAst 1"
--     it "Test fact with 1" $ do
--         let test = fact 1
--         let rest = show test
--         rest `shouldBe` "IntegerAst 1"
--     it "Test fact with 2" $ do
--         let test = fact 2
--         let rest = show test
--         rest `shouldBe` "IntegerAst 2"
--     it "Test fact with 13" $ do
--         let test = fact 13
--         let rest = show test
--         rest `shouldBe` "IntegerAst 6227020800"
