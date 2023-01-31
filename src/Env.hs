module ENV where
    import CPT
    import Data.Map
    import Prelude hiding (lookup)
    import Data.Typeable

    data Ast = Define String (Maybe Ast)
        | IntegerAst Int
        | SymbolAst String
        | Call [Ast]
        deriving Show

    type Env = Map String (Maybe Ast)

    envStorage :: Env
    envStorage = fromList []

    --printEnv :: Env -> IO ()
    --printEnv env = do
    --    print env

    updateEnv :: String -> Maybe Ast -> Env -> Env
    updateEnv key value env = insert key value env

    printAllEnv :: Env -> IO ()
    printAllEnv env = case t of
        t -> print t
        where t = toList env
    
    main :: IO ()
    main = do
        let env = updateEnv "x" (Just (IntegerAst 4)) envStorage
        env <- updateEnv "y" (Just (IntegerAst 5)) envStorage
        putStrLn $ show (typeOf env)
        --print the type of env
        -- printAllEnv env
        --print env storage
        