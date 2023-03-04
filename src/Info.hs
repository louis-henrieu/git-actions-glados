module Info (
    Ast(..),
    Env,
    getValueEnv,
    checkIfEmpty,
    eraseDoubles,
    Stack(..),
    ) where
    import Prelude hiding (lookup)
    import Prelude hiding (lookup)
    import Text.Show.Functions ()

    data Ast =  Define String (Ast)
        | DefineAlt [String] Ast
        -- => Lambda String [Ast]
        | IntegerAst Int 
        | FloatAst Float -- à rajouter dans parseCpt
        | SymbolAst String
        | Lambda [String] Ast
        | If Ast Ast Ast
        | ArgsLambda ([String], Ast)
        | Builtin ([Ast] -> Env -> Either String Ast)
        | Call [Ast]
        | Empty
        deriving (Show)

    
    --type Env =  String (Ast)
    type Env =  [(String ,Ast)]

    --getValueEnv :: Env -> String -> Either String Ast
    getValueEnv :: Env -> String -> Either String Ast
    getValueEnv [] key = Left ("Symbol \'" ++ key ++ "\' not found")
    getValueEnv env key = if key == fst (head env)
        then Right (snd (head env))
        else getValueEnv (tail env) key

    --checkIfEmpty :: [Ast] -> Bool
    checkIfEmpty :: [Ast] -> Bool
    checkIfEmpty [] = True
    checkIfEmpty (x:xs) = case x of
        Empty -> False
        _ -> checkIfEmpty xs

    eraseDoubles :: Env -> Env -> Env
    eraseDoubles [] env = env
    eraseDoubles (x:xs) env = if (fst x) `elem` (map fst env)
        then eraseDoubles xs env
        else eraseDoubles xs (x:env)
    
    data Stack = Stack {
        numFunctions :: Int, -- number of call_functions
        fast :: [String], -- local variables
        global :: [String], --  global variables
        constValue :: [Ast], -- constants
        bytecode :: [String], -- final bytecode
        end :: Bool -- end of the program
    }
