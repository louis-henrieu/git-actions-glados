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
        ("-", (Builtin preSub)),
        ("*", (Builtin preMul)),
        ("/", (Builtin preDiv)),
        ("mod", (Builtin preMod)),
        ("eq?", (Builtin preEqFunc))
        ]

    updateEnv :: String -> Ast -> Env -> Env
    updateEnv symbol ast env = (symbol, ast) : env

    replaceEnv :: String -> Ast -> [(String, Ast)] -> Env
    replaceEnv symbol ast [] = updateEnv symbol ast []
    replaceEnv symbol ast ((s, a):xs) = 
        if s == symbol then
            (symbol, ast) : xs
        else
            replaceEnv symbol ast xs

    updateAllEnv :: [String] -> [Ast] -> Env -> Env
    updateAllEnv [] [] env = env
    updateAllEnv (s:symbols) (a:asts) env = updateAllEnv symbols asts (updateEnv s a env)

    printAllEnv :: Env -> IO ()
    printAllEnv [] = return ()
    printAllEnv ((symbol, ast):xs) = do
        print (symbol, ast)
        printAllEnv xs