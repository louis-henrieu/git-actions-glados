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