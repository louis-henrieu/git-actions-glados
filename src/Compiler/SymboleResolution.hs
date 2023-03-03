module Compiler.SymboleResolution (
    resolution,
    replaceSymbol
) where 

--import Env (envStorage)
--import Ast
import Info (Ast(..), Env, getValueEnv)

safeUpdateEnv :: String -> Ast -> Env -> Env
safeUpdateEnv symbol ast curr_env = case curr_env of -- (symbol, ast) : curr_env
    [] ->  (symbol, ast):curr_env
    (x, _):n_env | symbol == x -> (x, ast) : (safeUpdateEnv symbol ast n_env)
    curr:n_env -> curr : (safeUpdateEnv symbol ast n_env)

-- | resolution :: [Ast] -> Env -> Env
--
-- Take a list of Ast and detects put values in Env
--
resolution :: [Ast] -> Env -> Env
resolution l curr_env = case l of
    [] -> curr_env
    (Define s a):n_l -> resolution n_l (safeUpdateEnv s a curr_env)

-- | replaceSymbol :: [Ast] -> Env -> Either String [Ast]
--
-- Replace all the symbole by their equivalent value in env
-- Error if symbole value isn't found in Env
--
replaceSymbol :: [Ast] -> Env -> Either String [Ast]
replaceSymbol ast_l env = case ast_l of
    [] -> Right ([])
    (Define s a) : n_l -> case (replaceSymbol n_l env) of
        Right (l) -> Right ((Define s a) : l)
        Left (x) -> Left (x)
    (SymbolAst s) : n_l -> case getValueEnv env s of
        Right (ast) -> case (replaceSymbol n_l env) of
            Right (l) -> Right (ast : l)
            Left (x) -> Left (x)
        Left (x) -> Left ("replaceSymbol ERROR : " ++ x)
    next : n_l -> case (replaceSymbol n_l env) of
        Right (l) -> Right (next : l)
        Left (x) -> Left (x)


