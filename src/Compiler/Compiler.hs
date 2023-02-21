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

