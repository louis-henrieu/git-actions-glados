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
    testPrintAllEnv

testEnvStorage :: Spec
testEnvStorage = describe "-- updateEnv --" $ do
    it "basic storage" $ do
        let test = envStorage
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>)]"

testUpdateEnv :: Spec
testUpdateEnv = describe "-- updateEnv --" $ do
    it "update storage" $ do
        let tEmpty = envStorage
        let test = updateEnv "apit" (IntegerAst 5) tEmpty
        let rest = show test
        rest `shouldBe` "[(\"apit\",IntegerAst 5),(\"+\",Builtin <function>),(\"-\",Builtin <function>),(\"*\",Builtin <function>),(\"/\",Builtin <function>),(\"mod\",Builtin <function>)]"

testPrintAllEnv :: Spec
testPrintAllEnv = describe "-- printAllEnv --" $ do
    it "\tEmpty env" $ do
        printAllEnv [] `shouldReturn` ()
    it "\tEnv with 1 element" $ do
        printAllEnv [("apit", (IntegerAst 5))] `shouldReturn` ()
    it "\tEnv with 2 elements" $ do
        printAllEnv [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))] `shouldReturn` ()
