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

printAst :: Ast -> IO ()
printAst ast = case ast of
    (IntegerAst i) -> print i
    (SymbolAst s) -> print s
    (FloatAst f) -> print f
    (Lambda ast1 ast2) -> print "#<procedure>"
    -- (CallAst l) -> print l
    -- (DefineAst s ast) -> print (s, ast)
    _ -> print "The Ast isn't valid"

someFunc :: Env -> IO ()
someFunc env = do
    line <- getLine
    if line == "quit" then return() else do
        let ast = FloatAst 42.0
        case preEvalAst ast (updateEnv "foo" (IntegerAst 42) env) of
            Right ast2 -> case evalAst ast2 (updateEnv "foo" (IntegerAst 42) env) of
                Right result -> case (fst result) of
                    Empty -> printAllEnv (snd result)
                    _ -> printAst (fst result)
                Left err -> print err
            Left err -> print err
        someFunc envStorage