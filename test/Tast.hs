module Tast (
    testAst
) where

-- Test import
import Test.Hspec

-- Project import
import Ast
import Info
import Cpt
import BasicFunc

testAst :: Spec
testAst = describe "\nTest all functions of Ast file" $ do
    testCheckOnlySymbols
    testParsingDefine
    testParsingList
    testPreEvalAst
    testLambdaFunc
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
    it "Ast empty env" $ do
        let test = parsingDefine (List [Symbol "define", Symbol "a", List []])
        let rest = show test
        rest `shouldBe` "Right (Define \"a\" (Call []))"
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
    it "should parse a define expression" $ do
        let input = List [Symbol "define", Symbol "x", Number 42]
        show (parsingList input) `shouldBe` "Right (Define \"x\" (IntegerAst 42))"
    it "parse a lambda with three elements" $ do
        let input = List [Symbol "lambda", Symbol "x", Number 42, Number 42]
        show (parsingList input) `shouldBe` "Right (Call [SymbolAst \"lambda\",SymbolAst \"x\",IntegerAst 42,IntegerAst 42])"
    it "should parse a lambda expression with one argument" $ do
        let input = List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]]
        show (parsingList input) `shouldBe` "Right (Lambda [\"x\"] (Call [SymbolAst \"+\",SymbolAst \"x\",IntegerAst 1]))"
    it "should parse a lambda expression with no arguments" $ do
        let input = List [Symbol "lambda", List [], Number 42]
        show (parsingList input) `shouldBe` "Right (Lambda [] (IntegerAst 42))"
    it "should parse an if expression" $ do
        let input = List [Symbol "if", Symbol "#t", Number 1, Number 0]
        show (parsingList input) `shouldBe` "Right (If (SymbolAst \"#t\") (IntegerAst 1) (IntegerAst 0))"
    it "should return an error for an invalid define expression" $ do
        let input = List [Symbol "define", Number 42]
        show (parsingList input) `shouldBe` "Left \"The define form is invalid\""
    it "should return an error for an invalid lambda expression" $ do
        let input = List [Symbol "lambda", Number 42, Number 42]
        show (parsingList input) `shouldBe` "Right (Call [SymbolAst \"lambda\",IntegerAst 42,IntegerAst 42])"
    it "should return an error for an invalid if expression" $ do
        let input = List [Symbol "if", Symbol "#t"]
        show (parsingList input) `shouldBe` "Right (Call [SymbolAst \"if\",SymbolAst \"#t\"])"
    it "returns Right (Lambda ...) when given a valid list with a lambda expression" $ do
        let input = List [Symbol "lambda", List [Symbol "x"], Symbol "x"]
        show (cptToAst input) `shouldBe` "Right (Lambda [\"x\"] (SymbolAst \"x\"))"
    it "should create a lambda expression with a symbol" $ do
      let symbols = ["x", "y"]
      show (cptToAst (List [Symbol "lambda", List (map Symbol symbols), Symbol "z"])) `shouldBe` "Right (Lambda [\"x\",\"y\"] (SymbolAst \"z\"))"
    it "lambda with a list of three symbols" $ do
        let test = cptToAst (List [Symbol "lambda", List [Symbol "x", Symbol "y", Symbol "z"], Symbol "z"])
        let rest = show test
        rest `shouldBe` "Right (Lambda [\"x\",\"y\",\"z\"] (SymbolAst \"z\"))"
    it "parsing not a list" $ do
        let test = parsingList (Symbol "a")
        let rest = show test
        rest `shouldBe` "Left \"Error in cptToAst: this is not a valid list\""


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
    --it "Call with a basic env" $ do
    --    let test = convertArgs [Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
    --    let rest = show test
    --    rest `shouldBe` "[IntegerAst 5,FloatAst 45.3,FloatAst 29.3]"
    --it "Call without env" $ do
    --    let test = convertArgs [Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]] []
    --    let rest = show test
    --    rest `shouldBe` "[Empty,FloatAst 45.3,FloatAst 29.3]"
    
    --it "Call with a basic env" $ do
    --    let test = convertArgs [Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
    --    let rest = show test
    --    test `shouldReturn` "The ast is : Call [IntegerAst 5,FloatAst 45.3,FloatAst 29.3]"
    --it "Call with a basic env and a symbol" $ do
    --    let test = convertArgs [Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3, SymbolAst "apit2"]] [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
    --    let rest = show test
    --    test `shouldReturn` "The ast is : Call [IntegerAst 5,FloatAst 45.3,FloatAst 29.3,IntegerAst 10]"


testLambdaFunc :: Spec
testLambdaFunc = describe "lambdaFunc" $ do
    let env = [
            ("#t", (IntegerAst 1)),
            ("#f", (IntegerAst 0)),
            ("+", (Builtin preAdd)),
            ("-", (Builtin preSub)),
            ("*", (Builtin preMul)),
            ("/", (Builtin preDiv)),
            ("mod", (Builtin preMod))
            ]
    it "should evaluate a lambda expression with one argument" $ do
        let test = lambdaFunc ["x"] (SymbolAst "x") [IntegerAst 42] []
        let rest = show test
        rest `shouldBe` "Left \"Lambda function needs at least one argument\""
    it "should evaluate a lambda expression with two arguments" $ do
        let test = lambdaFunc ["x", "y"] (Call [SymbolAst "+", SymbolAst "x", SymbolAst "y"]) [IntegerAst 2, IntegerAst 3] []
        let rest = show test
        rest `shouldBe` "Left \"Lambda function needs at least one argument\""
    it "should return an error if the number of arguments does not match the number of symbols" $ do
        let test = lambdaFunc ["x", "y"] (SymbolAst "+") [IntegerAst 1, IntegerAst 2] []
        let rest = show test
        rest `shouldBe` "Left \"Lambda function needs at least one argument\""
    it "should evaluate a lambda expression with two arguments with env" $ do
        let test = lambdaFunc ["x", "y"] (Call [SymbolAst "+", SymbolAst "x", SymbolAst "y"]) [IntegerAst 2, IntegerAst 3] env
        let rest = show test
        rest `shouldBe` "Right (FloatAst 5.0,[(\"mod\",Builtin <function>),(\"/\",Builtin <function>),(\"*\",Builtin <function>),(\"-\",Builtin <function>),(\"+\",Builtin <function>),(\"#f\",IntegerAst 0),(\"#t\",IntegerAst 1),(\"x\",IntegerAst 2),(\"y\",IntegerAst 3)])"

testPreEvalAst :: Spec
testPreEvalAst = describe "\t-- preEvalAst --" $ do
    it "Simple Symbol no env" $ do
        let test = preEvalAst (SymbolAst "a") []
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'a' not found\""
    it "Simple Number no env" $ do
        let test = preEvalAst (IntegerAst 10) []
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 10)"
    it "Simple NumberFloat no env" $ do
        let test = preEvalAst (FloatAst 10.2)  []
        let rest = show test
        rest `shouldBe` "Right (FloatAst 10.2)"
    it "Simple Symbol with env" $ do
        let test = preEvalAst (SymbolAst "a") [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'a' not found\""
    it "Simple Number with env" $ do
        let test = preEvalAst (IntegerAst 10) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 10)"
    it "Simple NumberFloat with env" $ do
        let test = preEvalAst (FloatAst 10.2) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 10.2)"
    it "Get env value" $ do
        let test = preEvalAst (SymbolAst "apit") [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 5)"
    it "Not implemented" $ do
        let test = preEvalAst (Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"apit\",FloatAst 45.3,FloatAst 29.3])"
    it "If with no env" $ do
        let test = preEvalAst (If (SymbolAst "#t") (IntegerAst 5) (IntegerAst 10)) []
        let rest = show test
        rest `shouldBe` "Right (If (SymbolAst \"#t\") (IntegerAst 5) (IntegerAst 10))"
    it "Lambda with no env" $ do
        let test = preEvalAst (Lambda ["x", "y"] (Call [SymbolAst "+", SymbolAst "x", SymbolAst "y"])) []
        let rest = show test
        rest `shouldBe` "Right (Lambda [\"x\",\"y\"] (Call [SymbolAst \"+\",SymbolAst \"x\",SymbolAst \"y\"]))"
    it "DefineAlt with no env" $ do
        let test = preEvalAst (DefineAlt ["apit"] (IntegerAst 5)) []
        let rest = show test
        rest `shouldBe` "Right (DefineAlt [\"apit\"] (IntegerAst 5))"

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
    it "If condition with env true" $ do
        let test = evalAst (If (SymbolAst "#t") (IntegerAst 5) (IntegerAst 10)) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 5,[(\"apit\",IntegerAst 5),(\"apit2\",IntegerAst 10)])"
    it "If condition with env false" $ do
        let test = evalAst (If (SymbolAst "#f") (IntegerAst 5) (IntegerAst 10)) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (IntegerAst 10,[(\"apit\",IntegerAst 5),(\"apit2\",IntegerAst 10)])"
    it "DefineAlt is in env" $ do
        let test = evalAst (DefineAlt ["apit"] (IntegerAst 5)) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",ArgsLambda ([],IntegerAst 5)),(\"apit2\",IntegerAst 10)])"
    it "DefineAlt is not in env" $ do
        let test = evalAst (DefineAlt ["apit"] (IntegerAst 5)) []
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",ArgsLambda ([],IntegerAst 5))])"
    it "Define" $ do
        let test = evalAst (Define "apit" (IntegerAst 5)) []
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",IntegerAst 5)])"
    it "Call is in env" $ do
        let test = evalAst (Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]) [("apit", (ArgsLambda ([], IntegerAst 5)))]
        let rest = show test
        rest `shouldBe` "Left \"Error in lambda - Invalid number of arguments\""
    it "Call is not in env" $ do
        let test = evalAst (Call [SymbolAst "apit", FloatAst 45.3, FloatAst 29.3]) []
        let rest = show test
        rest `shouldBe` "Left \"Symbol 'apit' not found\""
    it "Builtin withou env" $ do
        let test = evalAst (Call [SymbolAst "+", FloatAst 45.3, FloatAst 29.3]) []
        let rest = show test
        rest `shouldBe` "Left \"Symbol '+' not found\""
    it "Builtin with env" $ do
        let test = evalAst (Call [SymbolAst "+", FloatAst 45.3, FloatAst 29.3]) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "Left \"Symbol '+' not found\""
    it "Builtin is in env" $ do
        let test = evalAst (Call [SymbolAst "+", FloatAst 45.3, FloatAst 29.3]) [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10)), ("+", (ArgsLambda ([], IntegerAst 5)))]
        let rest = show test
        rest `shouldBe` "Left \"Error in lambda - Invalid number of arguments\""