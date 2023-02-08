module Tkeywords (
    testKeywords
) where

-- Test import
import Test.Hspec

-- Project import
import Keywords
import Info
import Env
import BasicFunc

testKeywords :: Spec
testKeywords = describe "\nTest all functions of Keywords file" $ do
    testPreEqFunc
    testEqFunc

testPreEqFunc :: Spec
testPreEqFunc = describe "-- preEqFunc --" $ do
    it "\tEmpty" $ do
        let storage = envStorage
        let test = preEqFunc [] storage
        let rest = show test
        rest `shouldBe` "Left \"If function needs at least three arguments\""
    it "\tSymbol without env" $ do
        let test = preEqFunc [SymbolAst "apit", SymbolAst "apit"] []
        let rest = show test
        rest `shouldBe` "Left \"args are not equal: apit || apit || \""
    it "\tInteger without env" $ do
        let test = preEqFunc [IntegerAst 5, IntegerAst 5] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "\tFloat without env" $ do
        let test = preEqFunc [FloatAst 5.5, FloatAst 5.5] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "\tInteger and Symbol without env" $ do
        let test = preEqFunc [IntegerAst 5, SymbolAst "apit"] []
        let rest = show test
        rest `shouldBe` "Left \"args are not equal: IntegerAst 5 || apit || \""
    it "\tFloat and Symbol without env" $ do
        let test = preEqFunc [FloatAst 5.5, SymbolAst "apit"] []
        let rest = show test
        rest `shouldBe` "Left \"args are not equal: FloatAst 5.5 || apit || \""
    it "\tInteger, Float and Symbol without env" $ do
        let test = preEqFunc [IntegerAst 5, FloatAst 5.5, SymbolAst "apit"] []
        let rest = show test
        rest `shouldBe` "Left \"If function needs at least three arguments\""
    it "\tInteger and Symbol with env" $ do
        let storage = envStorage
        let storage2 = updateEnv "apit" (IntegerAst 5) storage
        let test = preEqFunc [IntegerAst 5, SymbolAst "apit"] storage2
        let rest = show test
        rest `shouldBe` "Left \"args are not equal: IntegerAst 5 || apit || \""
    it "\tFloat and Symbol with env" $ do
        let storage = envStorage
        let storage2 = updateEnv "apit" (FloatAst 5.5) storage
        let test = preEqFunc [FloatAst 5.5, SymbolAst "apit"] storage2
        let rest = show test
        rest `shouldBe` "Left \"args are not equal: FloatAst 5.5 || apit || \""
    it "\tInteger, Float and Symbol with env" $ do
        let storage = envStorage
        let storage2 = updateEnv "apit" (IntegerAst 5) storage
        let test = preEqFunc [IntegerAst 5, FloatAst 5.5, SymbolAst "apit"] storage2
        let rest = show test
        rest `shouldBe` "Left \"If function needs at least three arguments\""

testEqFunc :: Spec
testEqFunc = describe "-- eqFunc --" $ do
    let env = [
            ("+", (Builtin preAdd)),
            ("-", (Builtin preSub)),
            ("*", (Builtin preMul)),
            ("/", (Builtin preDiv)),
            ("mod", (Builtin preMod)),
            ("eq?", (Builtin preEqFunc))
            ]
    it "\tEmpty" $ do
        let storage = envStorage
        let test = eqFunc [] storage
        let rest = show test
        rest `shouldBe` "Left \"If function needs at least three arguments\""
    it "\tInteger" $ do
        let test = eqFunc [IntegerAst 5, IntegerAst 5] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "\tInteger and Symbol with env" $ do
        let storage2 = updateEnv "apit" (IntegerAst 5) env
        let test = eqFunc [IntegerAst 5, IntegerAst 10] env
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"
    it "\tFloat" $ do
        let test = eqFunc [FloatAst 5.5, FloatAst 5.5] []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#t\")"
    it "\tFloat and Symbol with env" $ do
        let storage2 = updateEnv "apit" (FloatAst 5.5) env
        let test = eqFunc [FloatAst 5.5, FloatAst 10.5] env
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"#f\")"