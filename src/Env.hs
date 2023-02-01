module ENV where
    import CPT
    import Data.Map
    import Prelude hiding (lookup)
    import Data.Typeable

    data Ast = Define String (Ast)
        | IntegerAst Int
        | SymbolAst String
        | Call [Ast]
        deriving Show

    type Env = Map String (Ast)

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
    
    main :: IO ()
    main = do
        let env = updateEnv "x" ((IntegerAst 4)) envStorage
        let env2 = updateEnv "y" ((IntegerAst 5)) env
        print env
        print env2
        -- putStrLn $ show (typeOf env)
        --print the type of env
        -- printAllEnv env
        --print env storage
        