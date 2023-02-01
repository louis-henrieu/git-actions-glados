module Lib
    ( 
        next,
        --nextApp,
        --aaInt,
        getSh,
        someFunc
    ) where

import Temp
import Cpt
import Ast
import ParseCpt
import Env
import Info

next :: Int -> Int
next i = i + 1

--aaInt :: String -> Char
--aaInt (x:_) = x
--aaInt _ = ' '
--
getSh :: Char -> IO ()
getSh x = print (nexti x)


someFunc :: Env -> IO ()
someFunc = do
    line <- getLine
    if line == "quit" then return() else do
            let cpts = parseCpt line
            let cpt_ast = cptToAst (head cpts)
            print (show cpt_ast)
            -- case cpt_ast of
            --     Just ast -> case ast of
            --         (IntegerAst i) -> print i
            --         _ -> case evalAst ast of
            --             Right result -> case result of
            --                 (IntegerAst i) -> print i
            --                 _ -> print result
            --             Left err -> print err
            --     Nothing -> print "The Ast isn't valid"
            someFunc