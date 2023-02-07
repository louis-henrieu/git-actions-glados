module Tdefine
    (
        testDefine
    ) where

-- Test import
import Test.Hspec

-- Project import
import Define
import Info
import BasicFunc
import Keywords

testDefine :: Spec
testDefine = describe "\nTest all functions of Define file" $ do
    testIsInEnv
    testReplaceInEnv
    testDefineFunc

testIsInEnv :: Spec
testIsInEnv = describe "\t-- inEnv --" $ do
    let env = [
            ("+", (Builtin preAdd)),
            ("-", (Builtin preSub)),
            ("*", (Builtin preMul)),
            ("/", (Builtin preDiv)),
            ("mod", (Builtin preMod)),
            ("eq?", (Builtin preEqFunc))
            ]
    it "\tempty" $ do
        let test = isInEnv "apit" []
        let rest = show test
        rest `shouldBe` "False"
    it "\tfalse response" $ do
        let test = isInEnv "apit" env
        let rest = show test
        rest `shouldBe` "False"
    it "\ttrue response" $ do
        let test = isInEnv "+" env
        let rest = show test
        rest `shouldBe` "True"

testReplaceInEnv :: Spec
testReplaceInEnv = describe "\t-- replaceInEnv --" $ do
    let env = [
            ("+", (Builtin preAdd)),
            ("-", (Builtin preSub)),
            ("*", (Builtin preMul)),
            ("/", (Builtin preDiv)),
            ("mod", (Builtin preMod)),
            ("eq?", (Builtin preEqFunc))
            ]
    it "\tsimple builtin basic env" $ do
        let test = replaceInEnv "apit" (IntegerAst 10) env
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)]"
    it "\taddition simple basic env" $ do
        let test = replaceInEnv "+" (IntegerAst 10) env
        let rest = show test
        rest `shouldBe` "[(\"+\",IntegerAst 10),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)]"
    it "\tempty" $ do
        let test = replaceInEnv "apit" (IntegerAst 10) []
        let rest = show test
        rest `shouldBe` "[]"
    it "\tsoustraction with env with one entry" $ do
        let test = replaceInEnv "-" (IntegerAst 10) [(("+", (Builtin preAdd)))]
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>)]"

testDefineFunc :: Spec
testDefineFunc = describe "\t-- DefineFunc --" $ do
    let env = [
            ("+", (Builtin preAdd)),
            ("-", (Builtin preSub)),
            ("*", (Builtin preMul)),
            ("/", (Builtin preDiv)),
            ("mod", (Builtin preMod)),
            ("eq?", (Builtin preEqFunc))
            ]
    it "\tsimple test with env" $ do
        let test = defineFunc "apit" (IntegerAst 10) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",IntegerAst 10),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\tsimple test with existing func" $ do
        let test = defineFunc "+" (IntegerAst 10) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"+\",IntegerAst 10),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\ttest without env" $ do
        let test = defineFunc "apit" (IntegerAst 10) []
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",IntegerAst 10)])"
    it "\ttest with to parameters and one list" $ do
        let test = defineFunc "-" (IntegerAst 10) [(("+", (Builtin preAdd)))]
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"-\",IntegerAst 10),(\"+\",Builtin <function>)])"
    it "\t symbol ast with existing func" $ do
        let test = defineFunc "+" (SymbolAst "+") env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\t symbol ast with inexisting func" $ do
        let test = defineFunc "+" (SymbolAst "apit") env
        let rest = show test
        rest `shouldBe`  "Left \"Symbol 'apit' not found\""
    it "\t symbol ast is existing func" $ do
        let test = defineFunc "apit" (SymbolAst "+") env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",Builtin <function>),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\t lambda ast with existing func" $ do
        let test = defineFunc "+" (Lambda ["apit"] (SymbolAst "+")) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"+\",ArgsLambda ([\"apit\"],SymbolAst \"+\")),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\t lambda ast with inexisting func" $ do
        let test = defineFunc "+" (Lambda ["apit"] (SymbolAst "apit")) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"+\",ArgsLambda ([\"apit\"],SymbolAst \"apit\")),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\t lambda ast is existing func" $ do
        let test = defineFunc "apit" (Lambda ["apit"] (SymbolAst "+")) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",ArgsLambda ([\"apit\"],SymbolAst \"+\")),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\t lambda ast is inexisting func" $ do
        let test = defineFunc "apit" (Lambda ["apit"] (SymbolAst "apit")) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",ArgsLambda ([\"apit\"],SymbolAst \"apit\")),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"