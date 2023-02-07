module Main (main) where

--import Hspec
import Test.Hspec (hspec)

-- import test
import Test1
import Tcpt
import Tenv
import Tinfo
import Tbasicfunc
import Tkeywords
import Tdefine

main :: IO ()
main = hspec $ do
    test1
    testCpt
    testEnv
    testInfo
    testBasicFunc
    testKeywords
    testDefine