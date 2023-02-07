module Ast where
    import Cpt
    import Info
    import Env
    import Define

    -- cptToAst :: Cpt -> Either String Ast
    -- cptToAst (Number i) = Right (IntegerAst i)
    -- cptToAst (Symbol s) = Right (SymbolAst s)
    -- cptToAst (List (Symbol "lambda" : List l : cpt : [])) = case cptToAst cpt of
    --     Right ast -> Right (Lambda (map (\(Symbol s) -> s) l) ast)
    --     Left err -> Left err
    -- cptToAst (List (Symbol "lambda" : _)) = Left "Invalid lambda"
    -- cptToAst (List (Symbol "if" : cpt1 : cpt2 : cpt3 : [])) = case cptToAst cpt1 of
    --     Right ast1 -> case cptToAst cpt2 of
    --         Right ast2 -> case cptToAst cpt3 of
    --             Right ast3 -> Right (If ast1 ast2 ast3)
    --             Left err -> Left err
    --         Left err -> Left err
    --     Left err -> Left err
    -- cptToAst (List (Symbol "if" : _)) = Left "Invalid if"
    -- cptToAst (List (Symbol "define" : Symbol s : cpt : [])) = case cptToAst cpt of
    --     Right ast -> Right (Define s ast)
    --     Left err -> Left err
    -- cptToAst (List (Symbol "define" : _)) = Left "Invalid define"
    -- cptToAst (List l) = case t of
    --     t -> cptToAst t
    --     where t = List (Symbol "call" : l)

    lambdaFunc :: [String] -> Ast -> [Ast] -> Env -> Either String (Ast, Env)
    lambdaFunc _ _ _ [] = Left "Lambda function needs at least one argument"
    lambdaFunc symbols ast args env = if ((length symbols) == (length args))
        then do
            let env2 = updateAllEnv symbols args env
            case evalAst ast env2 of
                Right (ast, env3) -> Right (ast, env)
                Left err -> Left err
        else
            Left "Error in lambda - Invalid number of arguments"

    preEvalAst :: Ast -> Env -> Either String Ast
    preEvalAst (Define x y) env = Right (Define x y)
    preEvalAst (IntegerAst i) env = Right (IntegerAst i)
    preEvalAst (FloatAst f) env = Right (FloatAst f)
    preEvalAst (SymbolAst x) env = case getValueEnv env x of
        Right ast -> Right ast
        Left err -> Left err
    preEvalAst (Call c) env = Right (Call c)
    preEvalAst (Lambda x y) env = Right (Lambda x y)
    preEvalAst (DefineAlt x y) env = Right (DefineAlt x y)
    preEvalAst _ env = Left "Not implemented"

    evalAst :: Ast -> Env -> Either String (Ast, Env)
    evalAst (SymbolAst x) env = Right (SymbolAst x, env)
    evalAst (Define d x) env = defineFunc d x env
    evalAst (DefineAlt (n:args) ast) env = case isInEnv n env of
        True -> Right (Empty, (replaceEnv n (ArgsLambda (args, ast)) env []) )
        False -> Right (Empty, (n, ArgsLambda (args, ast)):env)
    evalAst (IntegerAst i) env = Right (IntegerAst i, env)
    evalAst (FloatAst f) env = Right (FloatAst f, env)
    evalAst (Call(SymbolAst "if":x:y:z:xs)) env = case length (y:z:xs) of
        2 -> case evalAst x env of
            Right (SymbolAst "#t", _) -> case preEvalAst y env of
                Right ast -> evalAst ast env
                Left err -> Left err
            Right (SymbolAst "#f", _) -> case preEvalAst z env of
                Right ast -> evalAst ast env
                Left err -> Left err
            Right _ -> Left "If condition must be a boolean"
            Left err -> Left err
        _ -> Left ("There is " ++ show (length xs) ++ "arguments after the if condition")
    evalAst (Call(SymbolAst x:xs)) env = case getValueEnv env x of
        Left err -> Left err
        Right res -> case res of
            ArgsLambda (x, y) -> lambdaFunc x y xs env
            Builtin f -> case f xs env of
                Left err -> Left err
                Right ast -> Right (ast, env)
            _ -> Left (x ++ " is not a function")
    evalAst (Lambda x y) env = Right(SymbolAst "#<procedure>", env)
    evalAst (Call((Lambda x y):z)) env = lambdaFunc x y z env
 
    -- evalAst (Lambda lx ly) env = Left("Need : Lambda")
    -- evalAst (If ix iy iz) env = Left("Need : If")
    -- evalAst (BuiltIn b) env = Left("Need : BuiltIn")
    -- evalAst (Call (SymbolAst f : xs)) env = case getValueEnv env f of
    --     Right x -> Right(Empty, env)
    --     Left err -> Left err