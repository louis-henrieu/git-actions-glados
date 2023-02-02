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

evalAll :: [Ast] -> Env -> IO ()
evalAll [] env = return ()
evalAll (x:xs) env =
    case preEvalAst x env of
        Right ast -> case evalAst ast env of
            Right result -> case (fst result) of
                Empty -> evalAll xs (snd result)
                _ -> printAst (fst result)
            Left err -> print (err)
        Left err -> print (err)


evalFunc :: Ast -> Env -> IO ()
evalFunc ast env = do
    case preEvalAst ast env of
        Right ast -> case evalAst ast env of
            Right result -> case (fst result) of
                Empty -> putStrLn "ok"
                _ -> printAst (fst result)
            Left err -> putStrLn("Error : " ++ err)
        Left err -> putStrLn("Error : " ++ err)
evalFunc _ _ = putStrLn "The Ast isn't valid"

someFunc :: Env -> IO ()
someFunc env = do
    -- Test scope :
    -- evalAll [Define "foo" (IntegerAst 42), SymbolAst "baz"] env

    -- test builtIn with add :
    putStrLn ("-------------")
    putStrLn  ("test add :\n")
    putStrLn ("test add 42 -20 :\nShould return 22")
    evalFunc (Call [SymbolAst "+", IntegerAst 42, IntegerAst (-20)]) env
    putStrLn "\n\n"
    putStrLn "test add foo 42\nfoo is defined as IntegerAst 42\nShould return 84"
    evalAll [Define "foo" (IntegerAst 42), Call [SymbolAst "+", SymbolAst "foo", IntegerAst 42]] env

    -- test builtIn with sub :
    putStrLn ("\n\n test sub :\n-------------\ntest sub 0 42 :\nShould return -42")
    evalFunc (Call [SymbolAst "-", IntegerAst 0, IntegerAst 42]) env
    putStrLn "\n\ntest sub foo 42\nfoo is defined as IntegerAst 42\nShould return 0"
    evalAll [Define "foo" (IntegerAst 42), Call [SymbolAst "-", SymbolAst "foo", IntegerAst 42]] env

    -- test builtIn with mul :
    putStrLn ("\n\ntest mul :\n-------------\ntest mul 0 42 :\nShould return 0")
    evalFunc (Call [SymbolAst "*", IntegerAst 2, IntegerAst 42]) env

    putStrLn "\n\ntest mul foo 42\nfoo is defined as IntegerAst 10\nShould return 420"
    evalAll [Define "foo" (IntegerAst 10), Call [SymbolAst "*", SymbolAst "foo", IntegerAst 42]] env

    -- test builtIn with div :
    putStrLn ("\n\ntest div :\n-------------\ntest div 0 42 :\nShould return 0")
    let test = Call [SymbolAst "/", IntegerAst 0, IntegerAst 42]
    evalFunc test env

    putStrLn "\n\ntest div 42 0 :\nShould return an error"
    evalFunc (Call [SymbolAst "/", IntegerAst 42, IntegerAst 0]) env

    putStrLn "\n\ntest modulo :\n-------------\ntest mod 42 0 :\n"
    evalFunc (Call [SymbolAst "mod", IntegerAst 42, IntegerAst 0]) env

    putStrLn "\n\ntest modulo 16 5 :\nShould return 1"
    evalFunc (Call [SymbolAst "mod", IntegerAst 16, IntegerAst 5]) env

    -- Test autre :
    -- line <- getLine
    -- if line == "quit" then return() else do
    --     let ast = FloatAst 42.0
    --     case preEvalAst ast (updateEnv "foo" (IntegerAst 42) env) of
    --         -- Right ast2 -> case evalAst ast2 (updateEnv "foo" (IntegerAst 42) env) of
    --             Right result -> case (fst result) of
    --                 Empty -> someFunc (snd result)
    --                 _ -> printAst (fst result)
    --             Left err -> print err
    --         Left err -> print err
    --     someFunc envStorage