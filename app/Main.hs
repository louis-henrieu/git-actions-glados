module Main (main) where

import Lib
import Env
import System.Environment

parseFile :: String -> Int -> String -> [String] -> [String]
parseFile [] 0 [] file = file
parseFile [] 0 line file = file ++ [line]
parseFile [] _ [] _ = error "Invalid file"
parseFile (x:xs) parenthesis line file = case x of
    '\0' -> case parenthesis of
        0 -> parseFile xs 0 [] (file ++ [line])
        _ -> error "Invalid file"
    '(' -> parseFile xs (parenthesis + 1) (line ++ [x]) file
    ')' -> parseFile xs (parenthesis - 1) (line ++ [x]) file
    '\t' -> parseFile xs parenthesis (line ++ [' ']) file
    '\n' -> case parenthesis of
        0 -> parseFile xs 0 [] (file ++ [line])
        _ -> parseFile xs parenthesis (line ++ [' ']) file
    _ -> parseFile xs parenthesis (line ++ [x]) file
parseFile w x y z = error("w : " ++ show w ++ "\nx : " ++ show x ++ "\ny : " ++ show y ++ "\nz : " ++ show z)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> do
            someFuncGetLine envStorage
        1 -> do
            file <- readFile (head args)
            someFuncFile envStorage (parseFile file 0 [] [])
        _ -> error "Invalid arguments"
