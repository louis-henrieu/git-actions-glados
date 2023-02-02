module Env where
    import Info
    import BasicFunc
    import Prelude hiding (lookup)
    import Data.Typeable
    import Info

    --data Ast = Define String (Ast)
    --    | IntegerAst Int
    --    | SymbolAst String
    --    | Call [Ast]
    --    -- | Builtin [Ast] -> Env -> Either String Ast
    --    deriving Show

    envStorage :: Env
    envStorage = []

    --printEnv :: Env -> IO ()
    --printEnv env = do
    --    print env

    updateEnv :: String -> Ast -> Env -> Env
    updateEnv symbol ast env = (symbol, ast) : env

    printAllEnv :: Env -> IO ()
    printAllEnv [] = return ()
    printAllEnv ((symbol, ast):xs) = do
        print (symbol, ast)
        printAllEnv xs

    initEnv :: Env -> Env
    initEnv env = do
        let env2 = updateEnv "+" (Builtin pre_add) env
        let env3 = updateEnv "-" (Builtin pre_sub) env2
        let env4 = updateEnv "*" (Builtin pre_mul) env3
        let env5 = updateEnv "/" (Builtin pre_div) env4
        let env6 = updateEnv "mod" (Builtin pre_mod) env5
        env6
    --initEnvMap :: EnvMap
    --initEnvMap = fromList [("+", (\args env -> Right sum args))]

    
    --main :: IO ()
    --main = do
    --    let env = updateEnv "x" ((IntegerAst 4)) envStorage
    --    let env2 = updateEnv "y" ((IntegerAst 5)) env
    --    print env
    --    print env2
        -- putStrLn $ show (typeOf env)
        --print the type of env
        -- printAllEnv env
        --print env storage
        