module Main (main) where

import Lib
import Env

main :: IO ()
main = someFunc (initEnv envStorage)
