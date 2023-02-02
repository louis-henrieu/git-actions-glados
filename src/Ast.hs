module Ast where
    import Cpt
    import Info
    import Env

    -- listCptToListAst' :: Cpt -> Ast
    -- listCptToListAst' l = case l of
    --     (List x) -> (Call (listCptToListAst' x))
    --     (List (Symbol "define"):(Symbol x):(Number y)) ->(Define x (Value y))
    --     (Number x)->(Value x)
    --     (Symbol x)->(Symb x)


    -- listCptToListAst :: [Cpt] -> [Ast]
    -- listCptToListAst l = case l of
    --     [] -> []
    --     (List x):l -> (Call (listCptToListAst x)) : listCptToListAst l
    --     (Symbol "define"):(Symbol x):(Number y):l
    --         ->(Define x (Value y)):listCptToListAst l
    --     (Number x):l
    --         ->(Value x):listCptToListAst l
    --     (Symbol x):l
    --         ->(Symb x):listCptToListAst l

    --fixAstList :: [Ast] -> [Maybe Ast]
    --fixAstList l = case l of
    --    [] -> []
    --    (Define x, y):(c):l -> Nothing
    --    (Define x, y):l -> fixAstList l
    --    (Call x) : l -> (fixAstList x) : fixAstList l


    --callListAst :: [Maybe Ast] -> Maybe Ast
    --callListAst l = case l of
    --    [] -> Nothing
    --    x -> Just (Call x)

    -- cptToAst :: Cpt -> Ast
    -- cptToAst (List (Symbol "define"):(Symbol x):y) = Define x (cptToAst y)
    -- cptToAst (List (Symbol "define", 
    -- cptToAst (Number x) = Just (Value x)
    -- cptToAst (Symbol x) = Just (Symb x)
    -- cptToAst (List x) = Just (Call (listCptToListAst x))

    createCall :: [Cpt] -> Either String [Ast]
    createCall l = case l of
        (x:xs) -> case cptToAst x of
            Right ast -> case createCall xs of
                Right asts -> Right (ast:asts)
                Left err -> Left err
            Left err -> Left err

    cptToAst :: Cpt -> Either String Ast
    cptToAst (Number i) = Right (IntegerAst i)
    cptToAst (Symbol s) = Right (SymbolAst s)
    cptToAst (List (Symbol "lambda" : cpt1 : cpt2 : [])) = case cptToAst cpt1 of
        Right ast1 -> case cptToAst cpt2 of
            Right ast2 -> Right (Lambda ast1 ast2)
            Left err -> Left err
        Left err -> Left err
    cptToAst (List (Symbol "lambda" : _)) = Left "Invalid lambda"
    cptToAst (List (Symbol "if" : cpt1 : cpt2 : cpt3 : [])) = case cptToAst cpt1 of
        Right ast1 -> case cptToAst cpt2 of
            Right ast2 -> case cptToAst cpt3 of
                Right ast3 -> Right (If ast1 ast2 ast3)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    cptToAst (List (Symbol "if" : _)) = Left "Invalid if"
    cptToAst (List (Symbol "define" : Symbol s : cpt : [])) = case cptToAst cpt of
        Right ast -> Right (Define s ast)
        Left err -> Left err
    cptToAst (List (Symbol "define" : _)) = Left "Invalid define"
    cptToAst (List l) = case t of
        t -> cptToAst t
        where t = List (Symbol "call" : l)

    -- defineValue :: Ast -> Env -> Env
    -- defineValue (Define x y) env = (x, y) : env
    -- defineValue _ env = env

    preEvalAst :: Ast -> Env -> Either String Ast
    preEvalAst (Define x y) env = Right (Define x y)
    preEvalAst (IntegerAst i) env = Right (IntegerAst i)
    preEvalAst (FloatAst f) env = Right (FloatAst f)
    preEvalAst (SymbolAst x) env = case getValueEnv env x of
        Right ast -> Right ast
        Left err -> Left err
    preEvalAst (Call c) env = Right (Call c)
    preEvalAst _ env = Left "Not implemented"
    -- preEvalAst (IntegerAst i) env = Right (IntegerAst i)
    -- preEvalAst (SymbolAst x) env = Right (SymbolAst x)
    -- preEvalAst (Lambda x y) env = Right (Lambda x y)
    -- preEvalAst (If x y z) env = Right (If x y z)
    -- preEvalAst (BuiltIn f) env = Right (BuiltIn f)
    -- preEvalAst (Call (SymbolAst f : xs)) env = case getValueEnv env f of
    --     Right (BuiltIn f) -> f xs env
    --     Right _ -> Left (f ++ " is not a function")
    --     Left err -> Left error "Error : " ++ err

    evalAst :: Ast -> Env -> Either String (Ast, Env)
    evalAst (Define d x) env = Right (Empty, updateEnv d x env)
    evalAst (IntegerAst i) env = Right (IntegerAst i, env)
    evalAst (FloatAst f) env = Right (FloatAst f, env)
    evalAst (Call(SymbolAst x:xs)) env = case getValueEnv env x of
        Left err -> Left err
        Right res -> case res of
            Builtin f -> case f xs env of
                Left err -> Left err
                Right ast -> Right (ast, env)
            _ -> Left (x ++ " is not a function")

    -- evalAst (Lambda lx ly) env = Left("Need : Lambda")
    -- evalAst (If ix iy iz) env = Left("Need : If")
    -- evalAst (BuiltIn b) env = Left("Need : BuiltIn")
    -- evalAst (Call (SymbolAst f : xs)) env = case getValueEnv env f of
    --     Right x -> Right(Empty, env)
    --     Left err -> Left err



    -- evalAst (Call (SymbolAst x : xs)) = case x of
    --     "+" -> Right (addFunction xs)
    --     "-" -> Right(minusFunction xs)
    --     "*" -> Right(mulFunction xs)
    --     "/" -> Right (divFunction xs)
    --     "%" -> Right (modFunction xs)
    --     _ -> Left ("Error : " ++ x ++ " is not a function")


    -- evalAst (Define x y) = Left("Need : Define")
    -- evalAst (SymbolAst x) = Left("Need :" ++ x ++ "Symbol to find")
    -- evalAst (Call []) = Left("Error : empty call")
    -- evalAst _ = Left("Error : Not a function")