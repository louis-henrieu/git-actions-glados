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
testCheckFloatInt = describe "Test check_float_int function" $ do
    it "Test check_float_int with empty list" $ do
        let test = check_float_int [] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with one element" $ do
        let test = check_float_int [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with one element" $ do
        let test = check_float_int [FloatAst 1.0] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with one element" $ do
        let test = check_float_int [SymbolAst "a"] [("a", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with one element" $ do
        let test = check_float_int [SymbolAst "a"] [("a", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with one element" $ do
        let test = check_float_int [SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with one element" $ do
        let test = check_float_int [SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 1)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with one element" $ do
        let test = check_float_int [SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 1.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with one element" $ do
        let test = check_float_int [SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, FloatAst 2.0] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, FloatAst 2.0] []
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [FloatAst 1.0, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right True"
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c"), ("c", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", IntegerAst 1), ("b", SymbolAst "c"), ("c", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c"), ("c", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test check_float_int with two elements" $ do
        let test = check_float_int [SymbolAst "a", SymbolAst "b"] [("a", FloatAst 1.0), ("b", SymbolAst "c"), ("c", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""

testPreAdd :: Spec
testPreAdd = describe "Test pre_add function" $ do
    it "Test pre_add with empty list" $ do
        let test = pre_add [] []
        let rest = show test
        rest `shouldBe` "Left \"Add function needs at least two arguments\""
    it "Test pre_add with one element" $ do
        let test = pre_add [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_add with two elements" $ do
        let test = pre_add [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 3)"
    it "Test pre_add with two elements and one symbol" $ do
        let test = pre_add [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_add with two elements and one symbol" $ do
        let test = pre_add [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_add with two elements and one symbol" $ do
        let test = pre_add [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_add with two elements and one symbol" $ do
        let test = pre_add [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_add with two elements and one symbol" $ do
        let test = pre_add [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_add with two elements and one symbol" $ do
        let test = pre_add [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""

testPreSub :: Spec
testPreSub = describe "Test pre_sub function" $ do
    it "Test pre_sub with empty list" $ do
        let test = pre_sub [] []
        let rest = show test
        rest `shouldBe` "Left \"Sub function needs at least two arguments\""
    it "Test pre_sub with one element" $ do
        let test = pre_sub [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_sub with two elements" $ do
        let test = pre_sub [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst (-1))"
    it "Test pre_sub with two elements and one symbol" $ do
        let test = pre_sub [IntegerAst 1, SymbolAst "-"] [("-", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_sub with two elements and one symbol" $ do
        let test = pre_sub [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_sub with two elements and one symbol" $ do
        let test = pre_sub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_sub with two elements and one symbol" $ do
        let test = pre_sub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_sub with two elements and one symbol" $ do
        let test = pre_sub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_sub with two elements and one symbol" $ do
        let test = pre_sub [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""

testPreMul :: Spec
testPreMul = describe "Test pre_mul function" $ do
    it "Test pre_mul with empty list" $ do
        let test = pre_mul [] []
        let rest = show test
        rest `shouldBe` "Left \"Mul function needs at least two arguments\""
    it "Test pre_mul with one element" $ do
        let test = pre_mul [IntegerAst 1] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_mul with two elements" $ do
        let test = pre_mul [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 2)"
    it "Test pre_mul with two elements and one symbol" $ do
        let test = pre_mul [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_mul with two elements and one symbol" $ do
        let test = pre_mul [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_mul with two elements and one symbol" $ do
        let test = pre_mul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_mul with two elements and one symbol" $ do
        let test = pre_mul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_mul with two elements and one symbol" $ do
        let test = pre_mul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_mul with two elements and one symbol" $ do
        let test = pre_mul [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""

testPreDiv :: Spec
testPreDiv = describe "Test pre_div function" $ do
    it "Test pre_div with empty list" $ do
        let test = pre_div [] []
        let rest = show test
        rest `shouldBe` "Left \"Div function needs at least two arguments\""
    --it "Test pre_div with one element" $ do
    --    let test = pre_div [IntegerAst 1] []
    --    let rest = show test
    --    rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_div with two elements" $ do
        let test = pre_div [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 0)"
    it "Test pre_div with two elements and one symbol" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_div with two elements and one symbol" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_div with two elements and one symbol" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_div with two elements and one symbol" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_div with two elements and one symbol" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_div with two elements and one symbol" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", SymbolAst "c")]
        let rest = show test
        rest `shouldBe` "Left \"The symbol 's' isn't valid\""
    it "Test pre_div with too many arguments" $ do
        let test = pre_div [IntegerAst 1, SymbolAst "a", IntegerAst 2] [("a", SymbolAst "b"), ("b", FloatAst 2.0), ("c", FloatAst 2.0)]
        let rest = show test  
        rest `shouldBe` "Left \"Div function only needs two arguments\"" 

testPreMod :: Spec
testPreMod = describe "Test pre_mod function" $ do
    it "Test pre_mod with empty list" $ do
        let test = pre_mod [] []
        let rest = show test
        rest `shouldBe` "Left \"Mod function needs at least two arguments\""
    --it "Test pre_mod with one element" $ do
    --    let test = pre_mod [IntegerAst 1] []
    --    let rest = show test
    --    rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_mod with two elements" $ do
        let test = pre_mod [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_mod with two elements and one symbol" $ do
        let test = pre_mod [IntegerAst 1, SymbolAst "a"] [("a", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 1)"
    it "Test pre_mod with two elements and one symbol" $ do
        let test = pre_mod [IntegerAst 1, SymbolAst "a"] [("a", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test pre_mod with two elements and one symbol" $ do
        let test = pre_mod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b")]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test pre_mod with two elements and one symbol" $ do
        let test = pre_mod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", IntegerAst 2)]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test pre_mod with two elements and one symbol" $ do
        let test = pre_mod [IntegerAst 1, SymbolAst "a"] [("a", SymbolAst "b"), ("b", FloatAst 2.0)]
        let rest = show test
        rest `shouldBe` "Left \"Mod function only works with integers\""
    it "Test pre_mod only needs two arguments" $ do
        let test = pre_mod [IntegerAst 1, SymbolAst "a", IntegerAst 2] [("a", SymbolAst "b"), ("b", FloatAst 2.0), ("c", FloatAst 2.0)]
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