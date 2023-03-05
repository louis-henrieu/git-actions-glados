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
    testUpdateAllEnv
    testReplaceAllEnv
    testPrintAllEnv

testEnvStorage :: Spec
testEnvStorage = describe "-- updateEnv --" $ do
    it "basic storage" $ do
        let test = envStorage
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"

testUpdateEnv :: Spec
testUpdateEnv = describe "-- updateEnv --" $ do
    it "update storage" $ do
        let tEmpty = envStorage
        let test = updateEnv "apit" (IntegerAst 5) tEmpty
        let rest = show test
        rest `shouldBe` "[(\"apit\",IntegerAst 5),(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "update storage" $ do
        let tEmpty = envStorage
        let test = updateEnv "apit" (IntegerAst 5) [("apit", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "[(\"apit\",IntegerAst 5),(\"apit\",IntegerAst 10)]"
    it "update symbol ast left" $ do
        let tEmpty = envStorage
        let test = updateEnv "apit" (SymbolAst "apit") [("tre", (IntegerAst 10))]
        let rest = show test
        rest `shouldBe` "[(\"apit\",SymbolAst \"apit\"),(\"tre\",IntegerAst 10)]"


testReplaceEnv :: Spec
testReplaceEnv = describe "-- replaceEnv --" $ do
    it "empty env" $ do
        let env = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) [] env 
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "replace storage" $ do
        let env = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) [("apit", (IntegerAst 10))] env 
        let rest = show test
        rest `shouldBe`  "[(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>),(\"apit\",IntegerAst 5)]"
    it "replace storage" $ do
        let env = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) [("apit2", (IntegerAst 10))] env 
        let rest = show test
        rest `shouldBe` "[(\"apit2\",IntegerAst 10),(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "replace storage" $ do
        let env = envStorage
        let test = replaceEnv "apit" (IntegerAst 5) [("apit2", (IntegerAst 10)), ("apit3", (IntegerAst 15))] env 
        let rest = show test
        rest `shouldBe` "[(\"apit3\",IntegerAst 15),(\"apit2\",IntegerAst 10),(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"

testUpdateAllEnv :: Spec
testUpdateAllEnv = describe "-- updateAllEnv --" $ do
    it "empty" $ do
        let test = updateAllEnv [] [] envStorage
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "new env" $ do
        let test = updateAllEnv ["apit", "apit2"] [(IntegerAst 5), (IntegerAst 10)] envStorage
        let rest = show test
        rest `shouldBe` "[(\"apit2\",IntegerAst 10),(\"apit\",IntegerAst 5),(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "no env" $ do
        let test = updateAllEnv ["apit", "apit2"] [(IntegerAst 5), (IntegerAst 10)] []
        let rest = show test
        rest `shouldBe` "[(\"apit2\",IntegerAst 10),(\"apit\",IntegerAst 5)]"
    it "all empty" $ do
        let test = updateAllEnv [] [] []
        let rest = show test
        rest `shouldBe` "[]"


testReplaceAllEnv :: Spec
testReplaceAllEnv = describe "-- replaceAllEnv --" $ do
    it "empty" $ do
        let test = replaceAllEnv [] [] envStorage
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "new env" $ do
        let test = replaceAllEnv ["apit", "apit2"] [(IntegerAst 5), (IntegerAst 10)] envStorage
        let rest = show test
        rest `shouldBe` "[(\"+\",Builtin <function>),(\"add\",Builtin <function>),(\"-\",Builtin <function>),(\"sub\",Builtin <function>),(\"*\",Builtin <function>),(\"mul\",Builtin <function>),(\"/\",Builtin <function>),(\"div\",Builtin <function>),(\"mod\",Builtin <function>),(\"eq?\",Builtin <function>),(\"==\",Builtin <function>),(\"<\",Builtin <function>),(\">\",Builtin <function>)]"
    it "no env" $ do
        let test = replaceAllEnv ["apit", "apit2"] [(IntegerAst 5), (IntegerAst 10)] []
        let rest = show test
        rest `shouldBe` "[]"
    it "all empty" $ do
        let test = replaceAllEnv [] [] []
        let rest = show test
        rest `shouldBe` "[]"

testPrintAllEnv :: Spec
testPrintAllEnv = describe "-- printAllEnv --" $ do
    it "\tEmpty env" $ do
        printAllEnv [] `shouldReturn` ()
    it "\tEnv with 1 element" $ do
        printAllEnv [("apit", (IntegerAst 5))] `shouldReturn` ()
    it "\tEnv with 2 elements" $ do
        printAllEnv [("apit", (IntegerAst 5)), ("apit2", (IntegerAst 10))] `shouldReturn` ()
