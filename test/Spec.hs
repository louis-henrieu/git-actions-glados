module Main (main) where

--import Hspec
import Test.Hspec (hspec)

-- import test
import Tcpt
import Tenv
import Tinfo
import Tbasicfunc
import Tkeywords
import Tdefine

main :: IO ()
main = hspec $ do
    testCpt
    testEnv
    testInfo
    testBasicFunc
    testKeywords
    testDefine