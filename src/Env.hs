module Env (
    envStorage,
    updateEnv,
    printAllEnv,
    replaceEnv,
    updateAllEnv
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
        ("==", (Builtin preEqFunc))
        ]

    updateEnv :: String -> Ast -> Env -> Env
    updateEnv symbol ast env = (symbol, ast) : env

    replaceEnv :: String -> Ast -> [(String, Ast)] -> [(String, Ast)] -> Env
    replaceEnv symbol ast [] env = env
    replaceEnv symbol ast ((s, a):xs) new_env = 
        if s == symbol then
            new_env ++ ((s, ast):xs)
        else
            replaceEnv symbol ast xs ((s, a):new_env)

    updateAllEnv :: [String] -> [Ast] -> Env -> Env
    updateAllEnv [] [] env = env
    updateAllEnv (s:symbols) (a:asts) env = updateAllEnv symbols asts (updateEnv s a env)

    printAllEnv :: Env -> IO ()
    printAllEnv [] = return ()
    printAllEnv ((symbol, ast):xs) = do
        print (symbol, ast)
        printAllEnv xs