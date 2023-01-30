module Main (main) where

import Test.Hspec (hspec)
--import Hspec

import Test1

main :: IO ()
main = hspec $ do
        test1
