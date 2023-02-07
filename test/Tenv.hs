module Tenv
    (
        testEnv
    ) where

-- Test import
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- Project import
import Cpt
import Ast
import Info
import Env
import BasicFunc

testEnv :: Spec
testEnv = describe "\nTest all functions of Env file" $ do
    testEnvStorage
    testUpdateEnv
    testReplaceEnv
    testPrintAllEnv

testEnvStorage :: Spec
testEnvStorage = describe "-- updateEnv --" $ do
    it "basic storage" $ do
        let test = envStorage
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)]"

testReplaceEnv :: Spec
testReplaceEnv = describe "-- replaceEnv --" $ do
    it "replace storage" $ do
        let tEmpty = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) tEmpty
        let rest = show test
        rest `shouldBe` "[(\"apit\",IntegerAst 5)]"
    it "replace storage with 2 elements" $ do
        let tEmpty = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) tEmpty
        let test2 = replaceEnv "apit2" (IntegerAst 10) test
        let rest = show test2
        rest `shouldBe` "[(\"apit2\",IntegerAst 10)]"
    it "replace storage with 3 elements" $ do
        let tEmpty = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) tEmpty
        let test2 = replaceEnv "apit2" (IntegerAst 10) test
        let test3 = replaceEnv "apit3" (IntegerAst 15) test2
        let rest = show test3
        rest `shouldBe` "[(\"apit3\",IntegerAst 15)]"
    it "replace storage with 4 elements" $ do
        let tEmpty = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) tEmpty
        let test2 = replaceEnv "apit2" (IntegerAst 10) test
        let test3 = replaceEnv "apit3" (IntegerAst 15) test2
        let test4 = replaceEnv "apit4" (IntegerAst 20) test3
        let rest = show test4
        rest `shouldBe` "[(\"apit4\",IntegerAst 20)]"
    it "replace by the same element" $ do
        let tEmpty = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) tEmpty
        let test2 = replaceEnv "apit" (IntegerAst 5) test
        let rest = show test2
        rest `shouldBe` "[(\"apit\",IntegerAst 5)]"

testUpdateEnv :: Spec
testUpdateEnv = describe "-- updateEnv --" $ do
    it "update storage" $ do
        let tEmpty = envStorage
        let test = updateEnv "apit" (IntegerAst 5) tEmpty
        let rest = show test
        rest `shouldBe` "[(\"apit\",IntegerAst 5),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>)]"

testPrintAllEnv :: Spec
testPrintAllEnv = describe "-- printAllEnv --" $ do
    it "\tEmpty env" $ do
        printAllEnv [] `shouldReturn` ()
    it "\tEnv with 1 element" $ do
        printAllEnv [("apit", (IntegerAst 5))] `shouldReturn` ()
    it "\tEnv with 2 elements" $ do
        printAllEnv [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))] `shouldReturn` ()
