module Lib
    ( 
        next,
        --nextApp,
        --aaInt,
        getSh,
        someFunc
    ) where

import Temp
import AST


next :: Int -> Int
next i = i + 1

--aaInt :: String -> Char
--aaInt (x:_) = x
--aaInt _ = ' '
--
getSh :: Char -> IO ()
getSh x = print (nexti x)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

getEnv :: k -> [(v, k)] -> Maybe v
getEnv _ [] = Nothing
getEnv k [(value, key):(vs, ks)] =
    | k == key = Just(value)
    | otherwise = getEnv k (vs, ks)

 eval :: IO()