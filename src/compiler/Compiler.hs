module Compiler where 

import Env
import Ast

safeUpdateEnv :: String -> Ast -> Env -> Env
safeUpdateEnv symbol ast env = case env of -- (symbol, ast) : env
    [] ->  (symbol, ast):env
    (x, y):env | symbol == x -> (x, ast) : (safeUpdateEnv symbol ast env)
    elem:env -> elem : (safeUpdateEnv symbol ast env)

resolution :: [Ast] -> Env -> Env
resolution l env = case l of
    [] -> env
    (Define s a):l -> resolution l (safeUpdateEnv s a env)

replaceSymbol :: [Ast] -> Env -> Either String [Ast]
replaceSymbol l env = case l of
    [] -> Right ([])
    (Define s a) : l -> (Define s a) : (replaceSymbol l env)
    (SymbolAst s) : l -> case getValueEnv env s of
        Right (ast) -> ast : (replaceSymbol l env)
        Left (x) -> Left ("replaceSymbol ERROR : " ++ x)
    x:l -> x : (replaceSymbol l env)


