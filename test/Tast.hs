module Tast (
    testAst
) where

-- Test import
import Test.Hspec

-- Project import
import Ast
import Info
import Cpt

testAst :: Spec
testAst = describe "\nTest all functions of Ast file" $ do
    testCheckOnlySymbols
    testParsingDefine
    testParsingList

testCheckOnlySymbols :: Spec
testCheckOnlySymbols = describe "Test checkOnlySymbols function" $ do
    it "Should return true if the list is empty" $ do
        checkOnlySymbols [] `shouldBe` Right True
    it "Should return true if the list contains only symbols" $ do
        checkOnlySymbols [Symbol "a", Symbol "b", Symbol "c"] `shouldBe` Right True
    it "Should return false if the list contains a non symbol" $ do
        checkOnlySymbols [Symbol "a", Symbol "b", Symbol "c", List []] `shouldBe` Left "Error :List [] is not a symbol"

testParsingDefine :: Spec
testParsingDefine = describe "Test parsingDefine function" $ do
    it "Should return a define ast if the list is valid" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", Symbol "b"])
        let rest = show test
        rest `shouldBe` "Right (Define \"a\" (SymbolAst \"b\"))"
    it "Should return a define ast if the list is valid" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", List [Symbol "b", Symbol "c"]])
        let rest = show test
        rest `shouldBe` "Right (Define \"a\" (Call [SymbolAst \"b\",SymbolAst \"c\"]))"
    it "Should return a define ast if the list is valid" $ do
        let test = parsingDefine (List [Symbol "define", List [Symbol "a", Symbol "b"], Symbol "c"])
        let rest = show test
        rest `shouldBe` "Right (DefineAlt [\"a\",\"b\"] (SymbolAst \"c\"))"
    it "Should return a define ast if the list is valid" $ do
        let test = parsingDefine (List [Symbol "define", List [Symbol "a", Symbol "b"], List [Symbol "c", Symbol "d"]])
        let rest = show test
        rest `shouldBe` "Right (DefineAlt [\"a\",\"b\"] (Call [SymbolAst \"c\",SymbolAst \"d\"]))"
    it "Should return an error if the list is invalid" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", Symbol "b", Symbol "c"])
        let rest = show test
        rest `shouldBe` "Left \"The define form is invalid\""
    it "Should return an error if the list is invalid" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a"])
        let rest = show test
        rest `shouldBe` "Left \"The define form is invalid\""
    it "Should return an error if the list is invalid" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", Symbol "b", List []])
        let rest = show test
        rest `shouldBe` "Left \"The define form is invalid\""
    it "Should return an error if the list is invalid" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", List []])
        let rest = show test
        rest `shouldBe` "Right (Define \"a\" (Call []))"
    it "Should return an error if the list is invalid" $ do
        let test = parsingDefine (List [Symbol "define", List []])
        let rest = show test
        rest `shouldBe` "Left \"The define form is invalid\""
    it "Not list parameter" $ do
        let test = parsingDefine (Symbol "a")
        let rest = show test
        rest `shouldBe` "Left \"The define form is invalid\""
    it "The ast part of the define is invalid (not a symbol or a list)" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", Number 10, NumberFloat 10.2])
        let rest = show test
        rest `shouldBe` "Left \"The define form is invalid\""

testParsingList :: Spec
testParsingList = describe "Test parsingList function" $ do
    it "Should return a list ast if the list is valid" $ do
        let test = parsingList (List [Symbol "a", Symbol "b", Symbol "c"])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"a\",SymbolAst \"b\",SymbolAst \"c\"])"
    it "Should return a list ast if the list is valid" $ do
        let test = parsingList (List [Symbol "a", Symbol "b", List [Symbol "c", Symbol "d"]])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"a\",SymbolAst \"b\",Call [SymbolAst \"c\",SymbolAst \"d\"]])"
    it "Should return a list ast if the list is valid" $ do
        let test = parsingList (List [Symbol "a", Symbol "b", List [Symbol "c", Symbol "d"], Symbol "e"])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"a\",SymbolAst \"b\",Call [SymbolAst \"c\",SymbolAst \"d\"],SymbolAst \"e\"])"
    it "Should return a list ast if the list is valid" $ do
        let test = parsingList (List [Symbol "a", Symbol "b", List [Symbol "c", Symbol "d"], Symbol "e", List [Symbol "f", Symbol "g"]])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"a\",SymbolAst \"b\",Call [SymbolAst \"c\",SymbolAst \"d\"],SymbolAst \"e\",Call [SymbolAst \"f\",SymbolAst \"g\"]])"
    it "Should return a list ast if the list is valid" $ do
        let test = parsingList (List [Symbol "a", Symbol "b", List [Symbol "c", Symbol "d"], Symbol "e", List [Symbol "f", Symbol "g"], Symbol "h"])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"a\",SymbolAst \"b\",Call [SymbolAst \"c\",SymbolAst \"d\"],SymbolAst \"e\",Call [SymbolAst \"f\",SymbolAst \"g\"],SymbolAst \"h\"])"
        