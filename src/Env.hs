module Env (
    envStorage,
    updateEnv,
    printAllEnv
) where
    import Info
    import BasicFunc
    import Prelude hiding (lookup)

    envStorage :: Env
    envStorage = [
        ("+", (Builtin pre_add)),
        ("-", (Builtin pre_sub)),
        ("*", (Builtin pre_mul)),
        ("/", (Builtin pre_div)),
        ("mod", (Builtin pre_mod))
        ]

    updateEnv :: String -> Ast -> Env -> Env
    updateEnv symbol ast env = (symbol, ast) : env

    printAllEnv :: Env -> IO ()
    printAllEnv [] = return ()
    printAllEnv ((symbol, ast):xs) = do
        print (symbol, ast)
        printAllEnv xs

        