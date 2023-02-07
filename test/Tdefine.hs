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
    testDefineFunc

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
    it "\tTest 1" $ do
        let test = defineFunc "apit" (IntegerAst 10) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",IntegerAst 10),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\tTest 2" $ do
        let test = defineFunc "+" (IntegerAst 10) env
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"+\",IntegerAst 10),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)])"
    it "\tTest 3" $ do
        let test = defineFunc "apit" (IntegerAst 10) []
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"apit\",IntegerAst 10)])"
    it "\tTest 4" $ do
        let test = defineFunc "-" (IntegerAst 10) [(("+", (Builtin preAdd)))]
        let rest = show test
        rest `shouldBe` "Right (Empty,[(\"-\",IntegerAst 10),(\"+\",Builtin <function>)])"