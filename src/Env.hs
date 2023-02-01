module Env where
    import Info
    import Data.Map
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
    envStorage = fromList []

    --printEnv :: Env -> IO ()
    --printEnv env = do
    --    print env

    updateEnv :: String -> Ast -> Env -> Env
    updateEnv key value env = insert key value env

    printAllEnv :: Env -> IO ()
    printAllEnv env = case t of
        t -> print t
        where t = toList env
    
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
        