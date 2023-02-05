module Info where
    import Prelude hiding (lookup)
    import Data.Typeable
    import Data.Data
    import Prelude hiding (lookup)
    import Text.Show.Functions

    data Ast = Define String (Ast)
        | IntegerAst Int
        | FloatAst Float
        | SymbolAst String
        -- | Function [Ast] -> Either String Ast
        | Lambda [String] Ast
        | If Ast Ast Ast
        | Builtin ([Ast] -> Env -> Either String Ast)
        | Call [Ast]
        | Empty
        deriving Show

    --type Env =  String (Ast)
    type Env =  [(String ,Ast)]

    --getValueEnv :: String -> Env -> Either String Ast
    getValueEnv :: Env -> String -> Either String Ast
    getValueEnv [] key = Left ("Symbole : \'" ++ key ++ "\' not found")
    getValueEnv env key = if key == fst (head env)
        then Right (snd (head env))
        else getValueEnv (tail env) key

    --convertArgs :: [Ast] -> Env -> [Ast]
    -- convert the symbol to the value
    convertArgs :: [Ast] -> Env -> [Ast]
    convertArgs [] _ = []
    convertArgs (arg:args) env = case arg of
        SymbolAst s -> case getValueEnv env s of
            Right x -> x : convertArgs args env
            Left err -> Empty : convertArgs args env
        _ -> arg : convertArgs args env

    --checkIfEmpty :: [Ast] -> Bool
    checkIfEmpty :: [Ast] -> Bool
    checkIfEmpty [] = True
    checkIfEmpty (x:xs) = case x of
        Empty -> False
        _ -> checkIfEmpty xs