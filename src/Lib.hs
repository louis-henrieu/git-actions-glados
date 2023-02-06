module Lib
    (
        --nextApp,
        --aaInt,
        someFunc
    ) where

import Temp
import Cpt
import Ast
import Env
import Info
import Keywords

printAst :: Ast -> IO ()
printAst ast = case ast of
    (IntegerAst i) -> putStrLn (show i)
    (SymbolAst s) -> putStrLn s
    (FloatAst f) -> print f
    (Lambda x ast2) -> print "#<procedure>"
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

someFunc :: Env -> IO ()
someFunc env = do

    printAllEnv env
    putStrLn ("-------------\n\ntest verify scope :\n")
    putStrLn ("test [(define foo 42), baz]\nShould return 42")
    evalAll [Define "foo" (IntegerAst 42), SymbolAst "baz"] env
    putStrLn ("-------------\ntest define [(define foo 42), (define baz 42), baz]\nShould return 42")
    evalAll [Define "foo" (IntegerAst 42), Define "baz" (IntegerAst 42), SymbolAst "baz"] env

    putStrLn ("-------------")
    putStrLn  ("test add :\n")
    putStrLn ("test add 42 -20 :\nShould return 22")
    evalFunc (Call [SymbolAst "+", IntegerAst 42, IntegerAst (-20)]) env
    putStrLn "\n\n"
    putStrLn "test add foo 42\nfoo is defined as IntegerAst 42\nShould return 84"
    evalAll [Define "foo" (IntegerAst 42), Call [SymbolAst "+", SymbolAst "foo", IntegerAst 42]] env

    putStrLn "\n\n"
    evalAll [Define "foo" (IntegerAst 42), Define "foo" (IntegerAst 0), Call [SymbolAst "+", SymbolAst "foo", IntegerAst 42]] env

    putStrLn ("\n\n test sub :\n-------------\ntest sub 0 42 :\nShould return -42")
    evalFunc (Call [SymbolAst "-", IntegerAst 0, IntegerAst 42]) env
    putStrLn "\n\ntest sub foo 42\nfoo is defined as IntegerAst 42\nShould return 0"
    evalAll [Define "foo" (IntegerAst 42), Call [SymbolAst "-", SymbolAst "foo", IntegerAst 42]] env

    putStrLn ("\n\ntest mul :\n-------------\ntest mul 0 42 :\nShould return 0")
    evalFunc (Call [SymbolAst "*", IntegerAst 2, IntegerAst 42]) env
    putStrLn "\n\ntest mul foo 42\nfoo is defined as IntegerAst 10\nShould return 420"
    evalAll [Define "foo" (IntegerAst 10), Call [SymbolAst "*", SymbolAst "foo", IntegerAst 42]] env

    putStrLn ("\n\ntest div :\n-------------\ntest div 0 42 :\nShould return 0")
    let test = Call [SymbolAst "/", IntegerAst 0, IntegerAst 42]
    evalFunc test env
    putStrLn "\n\ntest div 42 0 :\nShould return an error"
    evalFunc (Call [SymbolAst "/", IntegerAst 42, IntegerAst 0]) env

    putStrLn "\n\ntest modulo :\n-------------\ntest mod 42 0 :\n"
    evalFunc (Call [SymbolAst "mod", IntegerAst 42, IntegerAst 0]) env
    putStrLn "\n\ntest modulo 16 5 :\nShould return 1"
    evalFunc (Call [SymbolAst "mod", IntegerAst 16, IntegerAst 5]) env

    putStrLn "\n\ntest ConvertArgs :\n-------------\n"
    putStrLn (show (convertArgs [SymbolAst "foo", IntegerAst 42] [("foo", IntegerAst 42)]))

    putStrLn "\n\ntest eq :\n-------------\n"
    putStrLn "test eq 42 42 :\nShould return #t"
    evalFunc (Call [SymbolAst "eq?", IntegerAst 42, IntegerAst 42]) env
    putStrLn "\n\ntest eq 42 0 :\nShould return #f"
    evalFunc (Call [SymbolAst "eq?", IntegerAst 42, IntegerAst 0]) env

    putStrLn "\n\ntest if :\n-------------\n"
    putStrLn "test if (eq? 42 42) 42 0 :\nShould return 42"
    evalFunc (Call [SymbolAst "if", Call [SymbolAst "eq?", IntegerAst 42, IntegerAst 42], IntegerAst 42, IntegerAst 0]) env
    putStrLn "\n\ntest if (eq? 42 0) 0 (+ 2 2) :\nShould return 4"
    evalFunc (Call [SymbolAst "if", Call [SymbolAst "eq?", IntegerAst 42, IntegerAst 0], IntegerAst 0, Call [SymbolAst "+", IntegerAst 2, IntegerAst 2]]) env

    putStrLn "\n\ntest lambda :\n-------------\n (lambda (a b) (* a b)) 3 3\nShould return 9"
    evalFunc (Call[Lambda ["a", "b"] (Call[SymbolAst "*", SymbolAst "a", SymbolAst "b"]), IntegerAst 3, IntegerAst 3]) env
    putStrLn "\n\n(lambda (a b) (* a b)) should return a procedure"
    evalFunc (Lambda ["a", "b"] (Call[SymbolAst "*", SymbolAst "a", SymbolAst "b"])) env