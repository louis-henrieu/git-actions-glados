module Ast where
    import Cpt
    import Info
    import Env
    import Define

    checkOnlySymbols :: [Cpt] -> Either String Bool
    checkOnlySymbols [] = Right True
    checkOnlySymbols (x:xs) = case x of
        Symbol _ -> checkOnlySymbols xs
        _ -> Left ("Error :" ++ show x ++ " is not a symbol")

    parsingDefine :: Cpt -> Either String Ast
    parsingDefine (List (Symbol "define":Symbol s:xs)) = case length xs of
        1 -> case cptToAst (head xs) of
            Left err -> Left ("The ast part of the define is invalid: " ++ err)
            Right ast -> Right (Define s ast)
        _ -> Left "The define form is invalid"
    parsingDefine (List (Symbol "define":List x:xs)) = 
        if (length xs == 1) && (checkOnlySymbols x == Right True) then
            (case cptToAst (head xs) of
                Left err -> Left ("The ast part of the define is invalid: " ++ err)
                Right ast -> Right (DefineAlt (map (\ (Symbol s) -> s) x) ast))
        else
            Left "The define form is invalid"
    parsingDefine _ = Left "The define form is invalid"

    parsingList :: Cpt -> Either String Ast
    parsingList (List (Symbol "define":xs)) = parsingDefine (List (Symbol "define":xs))
    parsingList (List (Symbol "lambda":List x:xs)) = if length xs == 1 then 
        if null x then
            case cptToAst (head xs) of
                Left err -> Left ("The ast part of the lambda is invalid: " ++ err)
                Right ast -> case ast of
                    SymbolAst s -> Right (Lambda [] (SymbolAst s))
                    Call s -> Right (Lambda [] (Call s))
                    _ -> Left "The lambda form is invalid"
            else if checkOnlySymbols x == Right True then
                case cptToAst (head xs) of
                    Left err -> Left ("The ast part of the lambda is invalid: " ++ err)
                    Right ast -> case ast of
                        SymbolAst s -> Right (Lambda (map (\(Symbol s) -> s) x) (SymbolAst s))
                        Call s -> Right (Lambda (map (\(Symbol s) -> s) x) (Call s))
                        _ -> Left "The lambda form is invalid"
                else
                    Left "The lambda form is invalid"
        else
            Left "The lambda form is invalid"
    parsingList (List (Symbol "if":x:y:z:xs)) = if null xs then
        case cptToAst x of
            Left err -> Left ("The ast part of the if is invalid: " ++ err)
            Right ast -> case cptToAst y of
                Left err -> Left ("The ast part of the if is invalid: " ++ err)
                Right ast2 -> case cptToAst z of
                    Left err -> Left ("The ast part of the if is invalid: " ++ err)
                    Right ast3 -> Right (If ast ast2 ast3)
        else
            Left "The if form is invalid"
    parsingList (List s) = Right (Call (map (\x -> case cptToAst x of
        Left err -> error err
        Right ast -> ast) s))
    parsingList _ = Left "Error in cptToAst: this is not a valid list"

    cptToAst :: Cpt -> Either String Ast
    cptToAst (Number n) = Right (IntegerAst n)
    cptToAst (Symbol s) = Right (SymbolAst s)
    cptToAst (List x) = parsingList (List x)

    --convertArgs :: [Ast] -> Env -> [Ast]
    -- convert the symbol to the value
    convertArgs :: [Ast] -> Env -> [Ast]
    convertArgs [] _ = []
    convertArgs (arg:args) env = case arg of
        SymbolAst s -> case getValueEnv env s of
            Right x -> x : convertArgs args env
            Left err -> Empty : convertArgs args env
        Call x -> case preEvalAst (Call (convertArgs x env)) env of
            Right x -> case evalAst x env of
                Right (x, _) -> x : convertArgs args env
                Left err -> error ("Error in convertArgs: " ++ err ++ "\n args are : " ++ (show (arg:args)) ++ "\n env is : " ++ show env)
            Left err -> Empty : convertArgs args env
        _ -> arg : convertArgs args env

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
                Right (ast, env3) -> Right (ast, env3)
                Left err -> Left err
        else
            Left "Error in lambda - Invalid number of arguments"

    preEvalAst :: Ast -> Env -> Either String Ast
    preEvalAst (Define x y) env = Right (Define x y)
    preEvalAst (IntegerAst i) env = Right (IntegerAst i)
    preEvalAst (FloatAst f) env = Right (FloatAst f)
    preEvalAst (SymbolAst "#t") env = Right (SymbolAst "#t")
    preEvalAst (SymbolAst "#f") env = Right (SymbolAst "#f")
    preEvalAst (SymbolAst x) env = case getValueEnv env x of
        Right ast -> Right ast
        Left err -> Left err
    preEvalAst (If x y z) env = Right (If x y z)
    preEvalAst (Call c) env = Right (Call c)
    preEvalAst (Lambda x y) env = Right (Lambda x y)
    preEvalAst (DefineAlt x y) env = Right (DefineAlt x y)
    preEvalAst _ env = Left "Not implemented"

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
            Right (ast, env2) -> case ast of
                SymbolAst("#t") -> case preEvalAst y env2 of
                    Left err -> Left err
                    Right ast -> case evalAst ast env2 of
                        Left err -> Left err
                        Right (ast, env3) -> Right (ast, env3)
                SymbolAst("#f") -> case preEvalAst z env2 of
                    Left err -> Left err
                    Right ast -> case evalAst ast env2 of
                        Left err -> Left err
                        Right (ast, env3) -> Right (ast, env3)
                _ -> Left "Error in if"
    evalAst (Call(SymbolAst x : xs)) env = case getValueEnv env x of
        Left err -> Left err
        Right res -> case res of
            ArgsLambda (x, y) -> lambdaFunc x y xs env
            Builtin f -> case f (convertArgs xs env) env of
                Left err -> Left ("Error in builtin function " ++ x ++ " : " ++ err)
                Right ast -> Right (ast, env)
            _ -> Left (x ++ " is not a function")
    evalAst (Lambda x y) env = Right(SymbolAst "#<procedure>", env)
    evalAst (Call ((Lambda x y) : z)) env = lambdaFunc x y z env
    evalAst (Call (Builtin x : args)) env = case x args env of
        Left err -> error (show err)
        Right res -> Right (res, env)
    evalAst (Call (ArgsLambda (x ,y) : z)) env = lambdaFunc x y z env
    evalAst ast _ = error("The ast is : " ++ show(ast))