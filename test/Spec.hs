module Main (main) where

--import Hspec
import Test.Hspec (hspec)

-- import test
import Tast
import Tbasicfunc
import Tcpt
import Tcptast
import Tdefine
import Tenv
import Tinfo
import Tkeywords

main :: IO ()
main = hspec $ do
    testAst
    testBasicFunc
    testCpt
    testCptAst
    testDefine
    testEnv
    testInfo
    testKeywords