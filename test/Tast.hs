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
    testConvertArgs
    testEvalAst

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
    it "If condition" $ do
        let test = parsingList (List [Symbol "if", Symbol "a", Symbol "b", Symbol "c"])
        let rest = show test
        rest `shouldBe` "Right (If (SymbolAst \"a\") (SymbolAst \"b\") (SymbolAst \"c\"))"
    it "If with no symbol no argument" $ do
        let test = parsingList (List [Symbol "if"])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"if\"])"
    it "If with no symbol one argument" $ do
        let test = parsingList (List [Symbol "if", Symbol "a"])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"if\",SymbolAst \"a\"])"
    it "If with no symbol two arguements" $ do
        let test = parsingList (List [Symbol "if", Symbol "a", Symbol "b"])
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"if\",SymbolAst \"a\",SymbolAst \"b\"])"
    it "Number list parameter" $ do
        let test = parsingList (List [Number 10, Number 20, Number 30])
        let rest = show test
        rest `shouldBe` "Right (Call [IntegerAst 10,IntegerAst 20,IntegerAst 30])"

testConvertArgs :: Spec
testConvertArgs = describe "\t-- convertArgs --" $ do
    it "Simple Symbol no env" $ do
        let test = convertArgs [SymbolAst "a"] []
        let rest = show test
        rest `shouldBe` "[Empty]"
    it "Simple Number no env" $ do
        let test = convertArgs [IntegerAst 10] []
        let rest = show test
        rest `shouldBe` "[IntegerAst 10]"
    it "Simple NumberFloat no env" $ do
        let test = convertArgs [FloatAst 10.2]  []
        let rest = show test
        rest `shouldBe` "[FloatAst 10.2]"
    it "Simple Symbol with env" $ do
        let test = convertArgs [SymbolAst "a"] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "[Empty]"
    it "Simple Number with env" $ do
        let test = convertArgs [IntegerAst 10] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "[IntegerAst 10]"
    it "Simple NumberFloat with env" $ do
        let test = convertArgs [FloatAst 10.2] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "[FloatAst 10.2]"
    it "Get env value" $ do
        let test = convertArgs [SymbolAst "apit"] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "[IntegerAst 5]"
    --it "Call with env" $ do
    --    let test = convertArgs [Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
    --    let rest = show test
    --    rest `shouldBe` "[Call [Empty,Empty,Empty]]"
    --it "Call with env" $ do
    --    let test = convertArgs [Call [SymbolAst "a", SymbolAst "b", SymbolAst "c"]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
    --    let rest = show test
    --    rest `shouldBe` "[Call [Empty,Empty,Empty]]"
    --it "Call with env and symbol" $ do
    --    let test = convertArgs [Call [SymbolAst "a", SymbolAst "b", SymbolAst "c"]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10)), ("a", (IntegerAst 15))]
    --    let rest = show test
    --    rest `shouldBe` "[Call [IntegerAst 15,Empty,Empty]]"
    --it "get call env" $ do
    --    let test = convertArgs [Call [SymbolAst "a", SymbolAst "b", SymbolAst "c"]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10)), ("a", (Call [SymbolAst "a", SymbolAst "b", SymbolAst "c"]))]
    --    let rest = show test
    --    rest `shouldBe` "[Call [Call [Empty,Empty,Empty],Empty,Empty]]"


testEvalAst :: Spec
testEvalAst = describe "\t-- evalAst --" $ do
    it "Simple Symbol no env" $ do
        let test = evalAst (SymbolAst "a") []
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"a\",[])"
    it "Simple Number no env" $ do
        let test = evalAst (IntegerAst 10) []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 10,[])"
    it "Simple NumberFloat no env" $ do
        let test = evalAst (FloatAst 10.2)  []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 10.2,[])"
    it "Simple Symbol with env" $ do
        let test = evalAst (SymbolAst "a") [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"a\",[(\"apit\",IntegerAst 5),(\"apit2\",IntegerAst 10)])"
    it "Simple Number with env" $ do
        let test = evalAst (IntegerAst 10) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 10,[(\"apit\",IntegerAst 5),(\"apit2\",IntegerAst 10)])"
    it "Simple NumberFloat with env" $ do
        let test = evalAst (FloatAst 10.2) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 10.2,[(\"apit\",IntegerAst 5),(\"apit2\",IntegerAst 10)])"
    it "Get env value" $ do
        let test = evalAst (SymbolAst "apit") [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"apit\",[(\"apit\",IntegerAst 5),(\"apit2\",IntegerAst 10)])"
    it "Call with env" $ do
        let test = evalAst (Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Left \"apit is not a function\""