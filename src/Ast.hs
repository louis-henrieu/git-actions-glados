module Ast (
    convertArgs,
    preEvalAst,
    evalAst
) where
    import Info
    import Env
    import Define

    convertArgs :: [Ast] -> Env -> [Ast]
    convertArgs [] _ = []
    convertArgs (arg : args) env = case arg of
        SymbolAst s -> case getValueEnv env s of
            Right x -> x : convertArgs args env
            Left _ -> Empty : convertArgs args env
        Call x -> case preEvalAst (Call (convertArgs x env)) env of
            Right x' -> case evalAst x' env of
                Right (xs, _) -> xs : convertArgs args env
                Left _ -> Empty : convertArgs args env
            Left _ -> Empty : convertArgs args env
        _ -> arg : convertArgs args env

    lambdaFunc :: [String] -> Ast -> [Ast] -> Env -> Either String (Ast, Env)
    lambdaFunc _ _ _ [] = Left "Lambda function needs at least one argument"
    lambdaFunc symbols ast args env = if ((length symbols) == (length args))
        then do
            let env2 = updateAllEnv symbols (convertArgs args env) env
            case evalAst ast env2 of
                Right (ast2, env3) -> Right (ast2, (eraseDoubles env3 []))
                Left err -> Left err
        else
            Left "Error in lambda - Invalid number of arguments"

    preEvalAst :: Ast -> Env -> Either String Ast
    preEvalAst (Define x y) _ = Right (Define x y)
    preEvalAst (IntegerAst i) _ = Right (IntegerAst i)
    preEvalAst (FloatAst f) _ = Right (FloatAst f)
    preEvalAst (SymbolAst "#t") _ = Right (SymbolAst "#t")
    preEvalAst (SymbolAst "#f") _ = Right (SymbolAst "#f")
    preEvalAst (SymbolAst x) env = case getValueEnv env x of
        Right ast -> Right ast
        Left err -> Left err
    preEvalAst (If x y z) _ = Right (If x y z)
    preEvalAst (Call c) _ = Right (Call c)
    preEvalAst (Lambda x y) _ = Right (Lambda x y)
    preEvalAst (DefineAlt x y) _ = Right (DefineAlt x y)
    preEvalAst _ _ = Left "Not implemented"

    evalAst :: Ast -> Env -> Either String (Ast, Env)
    evalAst (SymbolAst x) env = Right (SymbolAst x, env)
    evalAst (Define d x) env = defineFunc d x env
    evalAst (DefineAlt (n : args) ast) env = case isInEnv n env of
        True -> Right (Empty, (replaceEnv n (ArgsLambda (args, ast)) env []) )
        False -> Right (Empty, (n, ArgsLambda (args, ast)):env)
    evalAst (IntegerAst i) env = Right (IntegerAst i, env)
    evalAst (FloatAst f) env = Right (FloatAst f, env)
    evalAst (If x y z) env = case preEvalAst x env of
        Left err -> Left err
        Right ast -> case evalAst ast env of
            Left err -> Left err
            Right (c_ast, env2) -> case c_ast of
                SymbolAst("#t") -> case preEvalAst y env2 of
                    Left err -> Left err
                    Right r_ast -> case evalAst r_ast env2 of
                        Left err -> Left err
                        Right (f_ast, env3) -> Right (f_ast, env3)
                SymbolAst("#f") -> case preEvalAst z env2 of
                    Left err -> Left err
                    Right r_ast -> case evalAst r_ast env2 of
                        Left err -> Left err
                        Right (f_ast, env3) -> Right (f_ast, env3)
                _ -> Left "Error in if"
    evalAst (Call(SymbolAst x : xs)) env = case getValueEnv env x of
        Left err -> Left err
        Right res -> case res of
            ArgsLambda (f_x, y) -> lambdaFunc f_x y xs env
            Builtin f -> case f (convertArgs xs env) env of
                Left err -> Left ("Error in builtin function " ++ x ++ " : " ++ err ++ "!")
                Right ast -> Right (ast, env)
            _ -> Left (x ++ " is not a function")
    evalAst (Lambda (x : xs) y) env = defineFunc x (Lambda (xs) y) env
    evalAst (ArgsLambda _) env = Right(SymbolAst "#<procedure>", env)
    evalAst (Call ((Lambda (x : xs) y) : z)) env = defineFunc x (Lambda xs y) env
    evalAst (Call (Builtin x : args)) env = case x args env of
        Left err -> error (show err)
        Right res -> Right (res, env)
    evalAst (Call (ArgsLambda (x, y) : z)) env = lambdaFunc x y z env
    evalAst (Builtin _) env = Right (SymbolAst"#<procedure>", env) 
    evalAst ast _ = error("The ast is : " ++ show(ast))