module Lib
    ( 
        next,
        --nextApp,
        --aaInt,
        getSh,
        someFunc
    ) where

import Temp


next :: Int -> Int
next i = i + 1

--aaInt :: String -> Char
--aaInt (x:_) = x
--aaInt _ = ' '
--
getSh :: Char -> IO ()
getSh x = print (nexti x)


someFunc :: IO ()
someFunc = getSh 'c'