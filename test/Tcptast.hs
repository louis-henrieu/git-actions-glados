module Tcptast (
    testCptAst
) where

-- import Hspec
import Test.Hspec

-- Project import
import CptAst
import Info
import Cpt

testCptAst :: Spec
testCptAst = describe "\nTest all functions of CptAst file" $ do
    testCptToAst
    testCptToAstList

testCptToAst :: Spec
testCptToAst = describe "\t--cptToAst --" $ do
    it "cptToAst float" $ do
        let test = cptToAst (NumberFloat 1.0)
        let rest = show test
        rest `shouldBe` "FloatAst 1.0"
    it "cptToAst int" $ do
        let test = cptToAst (Number 20)
        let rest = show test
        rest `shouldBe` "IntegerAst 20"
    it "cptToAst symbol" $ do
        let test = cptToAst (Symbol "test")
        let rest = show test
        rest `shouldBe` "SymbolAst \"test\""
    it "cptToAst list" $ do
        let test = cptToAst (List [Number 20, NumberFloat 1.0])
        let rest = show test
        rest `shouldBe` "Call [IntegerAst 20,FloatAst 1.0]"
        

testCptToAstList :: Spec
testCptToAstList = describe "\t--cptToAstList --" $ do
    it "cptToAstList empty list" $ do
        let test = cptToAstList []
        let rest = show test
        rest `shouldBe` "Left \"Holala ! Can't parse an empty list\""
    it "cptToAstList single float" $ do
        let test = cptToAstList [NumberFloat 1.0]
        let rest = show test
        rest `shouldBe` "Right (FloatAst 1.0)"
    it "cptToAstList single int" $ do
        let test = cptToAstList [Number 20]
        let rest =  show test
        rest `shouldBe` "Right (IntegerAst 20)"
    it "cptToAstList single symbol" $ do
        let test = cptToAstList [Symbol "test"]
        let rest = show test
        rest `shouldBe` "Right (SymbolAst \"test\")"
    it "cptToAstList single list" $ do
        let test = cptToAstList [List [Number 20, NumberFloat 1.0]]
        let rest = show test
        rest `shouldBe` "Right (Call [IntegerAst 20,FloatAst 1.0])"
    it "cptToAstList with good separator but not symbol" $ do
        let test = cptToAstList [Number 20, Separator "=>", Number 20]
        let rest = show test
        rest `shouldBe` "Left \"Holala ! The first element of the list must be a symbol\""
    it "cptToAstList symbol with good separator" $ do
        let test = cptToAstList [Symbol "test", Separator "=>", Number 20]
        let rest = show test
        rest `shouldBe` "Right (Define \"test\" (IntegerAst 20))"
    it "basic function yuyu" $ do
        let test = cptToAstList [Symbol "yuyu", Separator "=>", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]]]
        let rest = show test
        rest `shouldBe` "Right (Define \"yuyu\" (Call [SymbolAst \"lambda\",Call [SymbolAst \"x\"],Call [SymbolAst \"+\",SymbolAst \"x\",IntegerAst 1]]))"
    it "basic function yuyu with bad separator" $ do
        let test = cptToAstList [Symbol "yuyu", Separator "=>", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]], Separator "=>"]
        let rest = show test
        rest `shouldBe` "Left \"Holala ! There is a length of 4\""
    it "separator is the first element of the list" $ do
        let test = cptToAstList [Separator "=>", Symbol "yuyu", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]]]
        let rest = show test
        rest `shouldBe` "Left \"Holala ! Seperator can't be the first element !!!\""
    it "function who take an argument in parameter" $ do
        let test = cptToAstList [Symbol "yuyu", Symbol "x", Separator "=>", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]], Symbol "yuyu", Separator "=>", Number 20]
        let rest = show test
        rest `shouldBe` "Right (Lambda [\"yuyu\",\"x\"] (Call [SymbolAst \"lambda\",Call [SymbolAst \"x\"],Call [SymbolAst \"+\",SymbolAst \"x\",IntegerAst 1]]))"
    it "function who take an argument in parameter left" $ do
        let test = cptToAstList [Symbol "yuyu", Symbol "x", Separator "=>", List [Symbol "lambda", List [Symbol "x"], List [Symbol "+", Symbol "x", Number 1]], Symbol "yuyu", Separator "=>"]
        let rest = show test
        rest `shouldBe` "Right (Lambda [\"yuyu\",\"x\"] (Call [SymbolAst \"lambda\",Call [SymbolAst \"x\"],Call [SymbolAst \"+\",SymbolAst \"x\",IntegerAst 1]]))"
    it "should return a Right Prrr when given a valid Prrr statement" $ do
        let test = cptToAstList [Symbol "Prrr", Symbol "x", Symbol "of", List [Symbol "a", Symbol "b"], List [Symbol "+", Number 1, Number 2]]
        let rest = show test
        rest `shouldBe` "Left \"Holala 1 ! There is a problem with the number of arguments in the Prrr statement\""
    it "function Prrr x of three arguments" $ do
        let test = cptToAstList [Symbol "Prrr", Symbol "x", Symbol "of", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"], Symbol "yuyu", Separator "=>", Number 20]
        let rest = show test
        rest `shouldBe` "Left \"Holala ! There is a non symbol in the list of arguments ! Argument is : List [Symbol \\\"a\\\",Symbol \\\"b\\\"]and i is 3\""
    it "function Prrr x of two arguments" $ do
        let test = cptToAstList [Symbol "Prrr", Symbol "x", Symbol "of", List [Symbol "+", Symbol "x", Number 1]]
        let rest = show test
        rest `shouldBe` "Left \"Holala ! There can only be statement as list in the Prrr statement\""
    it "should return a Left error message when given an invalid Prrr statement" $ do
        let test = cptToAstList [Symbol "Prrr", Symbol "x", Symbol "of"]
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"Prrr\",SymbolAst \"x\",SymbolAst \"of\"])"
    it "should return a Right Call when given a list of symbols" $ do
        let test = cptToAstList [Symbol "+", Number 1, Number 2]
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"+\",IntegerAst 1,IntegerAst 2])"
    it "function prr x of end with list of arguments" $ do
        let test = cptToAstList [Symbol "Prrr", Symbol "x", Symbol "of", List [Symbol "a", Symbol "b"]]
        let rest = show test
        rest `shouldBe` "Left \"Holala ! There can only be statement as list in the Prrr statement\""
    it "list of two symbol" $ do
        let test = cptToAstList [Symbol "yuyu", Symbol "x"]
        let rest = show test
        rest `shouldBe` "Right (Call [SymbolAst \"yuyu\",SymbolAst \"x\"])"
    it "list of symbol and list" $ do
        let test = cptToAstList [Symbol "Prrr", Symbol "x", Symbol "of", List [Symbol "a", Symbol "b"], List [Symbol "+", Number 1, Number 2]]
        let rest = show test
        rest `shouldBe` "Left \"Holala 1 ! There is a problem with the number of arguments in the Prrr statement\""
    it "uniqaly three numbers" $ do
        let test = cptToAstList [Number 1, Number 2, Number 3]
        let rest = show test
        rest `shouldBe` "Right (Call [IntegerAst 1,IntegerAst 2,IntegerAst 3])"