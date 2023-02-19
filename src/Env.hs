module Env (
    envStorage,
    updateEnv,
    printAllEnv,
    replaceEnv,
    updateAllEnv,
    replaceAllEnv
) where
    import Info
    import BasicFunc
    import Prelude hiding (lookup)
    import Keywords

    envStorage :: Env
    envStorage = [
        ("+", (Builtin preAdd)),
        ("add", (Builtin preAdd)),
        ("-", (Builtin preSub)),
        ("sub", (Builtin preSub)),
        ("*", (Builtin preMul)),
        ("mul", (Builtin preMul)),
        ("/", (Builtin preDiv)),
        ("div", (Builtin preDiv)),
        ("mod", (Builtin preMod)),
        ("eq?", (Builtin preEqFunc)),
        ("==", (Builtin preEqFunc)),
        ("<", (Builtin preInf)),
        (">", (Builtin preSup))
        ]

    updateEnv :: String -> Ast -> Env -> Env
    updateEnv symbol ast env = case ast of
        SymbolAst s -> case getValueEnv env s of
            Right _c
             -> error ("Error: " ++ s ++ " is not a value")
            Left _ -> (symbol, ast):env
        _ -> (symbol, ast):env

    replaceEnv :: String -> Ast -> [(String, Ast)] -> [(String, Ast)] -> Env
    replaceEnv _ _ [] env = env
    replaceEnv symbol ast ((s, a):xs) new_env = 
        if s == symbol then
            new_env ++ ((s, ast):xs)
        else
            replaceEnv symbol ast xs ((s, a):new_env)

    updateAllEnv :: [String] -> [Ast] -> Env -> Env
    updateAllEnv [] [] env = env
    updateAllEnv (s:symbols) (a:asts) env = updateAllEnv symbols asts (updateEnv s a env)
    updateAllEnv _ _ _ = []

    replaceAllEnv :: [String] -> [Ast] -> Env -> Env
    replaceAllEnv [] [] env = env
    replaceAllEnv (s:symbols) (a:asts) env = replaceAllEnv symbols asts (replaceEnv s a env [])
    replaceAllEnv _ _ _ = []

    printAllEnv :: Env -> IO ()
    printAllEnv [] = return ()
    printAllEnv ((symbol, ast):xs) = do
        print (symbol, ast)
        printAllEnv xs