module BasicFunc (
    preAdd,
    preSub,
    preMul,
    preDiv,
    preMod,
    preInf,
    preSup,
    checkZero,
    checkFloatInt
) where

    import Info

    convertFloatInt :: Ast -> Ast
    convertFloatInt (FloatAst f) = if f == (fromIntegral (round f :: Integer)) then 
            IntegerAst (round (f)) 
        else 
            Empty
    convertFloatInt _ = Empty

    convertFloatIntList :: [Ast] -> [Ast]
    convertFloatIntList [] = []
    convertFloatIntList (x:xs) = convertFloatInt (x) : convertFloatIntList xs

    convertIntFloat :: [Ast] -> [Ast]
    convertIntFloat [] = []
    convertIntFloat (x:xs) = case x of
        (IntegerAst number) -> FloatAst (fromIntegral number) : convertIntFloat xs
        ast -> ast : convertIntFloat xs

    checkFloatInt :: [Ast] -> Env -> Either String Bool
    checkFloatInt [] _ = Right True
    checkFloatInt (x:xs) env = case x of
        (IntegerAst _) -> checkFloatInt xs env
        (FloatAst _) -> checkFloatInt xs env
        (SymbolAst s) -> case getValueEnv env s of
            Right ast -> case ast of
                (IntegerAst _) -> checkFloatInt xs env
                (FloatAst _) -> checkFloatInt xs env
                _ -> Left ("Not a number" ++ show ast ++ " \n env is " ++ show env)
            Left err -> Left err
        _ -> Left "Not a number"
    
    preAdd :: [Ast] -> Env -> Either String Ast
    preAdd [] _ = Left "Add function needs at least two arguments"
    preAdd args env = case checkIfEmpty args of
        True -> case checkFloatInt args env of
            Right True -> add (convertIntFloat(args)) env
            Right False -> Left ("There is at least one float" ++ show args)
            Left err -> Left err
        False -> Left ("There is at least one symbol that isn't defined" ++ show args)

    add :: [Ast] -> Env -> Either String Ast
    add [] _ = Right (IntegerAst 0)
    add (x:xs) _ = case x of
        (IntegerAst i) -> Right (IntegerAst (i + (sum [is | IntegerAst is <- xs])))
        (FloatAst f) -> Right (FloatAst (f + (sum [fs | FloatAst fs <- xs])))
        _ -> Left "Not a number"

    preSub :: [Ast] -> Env -> Either String Ast
    preSub [] _ = Left "Sub function needs at least two arguments"
    preSub args env = case checkIfEmpty args  of
        True -> case checkFloatInt args env of
            Right True -> sub (convertIntFloat(args)) env
            Right False -> Left ("There is at least one float" ++ show args)
            Left err -> Left err
        False -> Left ("There is at least one symbol that isn't defined" ++ show args)

    sub :: [Ast] -> Env -> Either String Ast
    sub [] _ = Right (IntegerAst 0)
    sub (x:xs) _ = case x of
        (IntegerAst i) -> Right (IntegerAst (i - (sum [is | IntegerAst is <- xs])))
        (FloatAst f) -> Right (FloatAst (f - (sum [fs | FloatAst fs <- xs])))
        _ -> Left "Not a number"

    preMul :: [Ast] -> Env -> Either String Ast
    preMul [] _ = Left "Mul function needs at least two arguments"
    preMul args env = case checkIfEmpty args of
        True -> case checkFloatInt args env of
            Right True -> mul (convertIntFloat(args)) env
            Right False -> Left ("There is at least one float" ++ show args)
            Left err -> Left err
        False -> Left ("There is at least one symbol that isn't defined" ++ show args)

    mul :: [Ast] -> Env -> Either String Ast
    mul [] _ = Right (IntegerAst 1)
    mul (x:xs) _ = case x of
        (IntegerAst i) -> Right (IntegerAst (i * (product [is | IntegerAst is <- xs])))
        (FloatAst f) -> Right (FloatAst (f * (product [fs | FloatAst fs <- xs])))
        _ -> Left "Not a number"

    checkZero :: Ast -> Env -> Either String Ast
    checkZero (IntegerAst i) _ = case i of
        0 -> Left "Second argument is not allowed to be a 0"
        _ -> Right (IntegerAst i)
    checkZero (FloatAst f) _ = case f of
        0 -> Left "Second argument is not allowed to be a 0"
        _ -> Right (FloatAst f)
    checkZero (SymbolAst s) env = case getValueEnv env s of
        Right ast -> case ast of
            (IntegerAst i) -> case i of
                0 -> Left "Second argument is not allowed to be a 0"
                _ -> Right (IntegerAst i)
            (FloatAst f) -> case f of
                0 -> Left "Second argument is not allowed to be a 0"
                _ -> Right (FloatAst f)
            _ -> Left "The symbol \'s\' isn't valid"
        Left err -> Left err
    checkZero _ _ = Left "Not a number"

    preDiv :: [Ast] -> Env -> Either String Ast
    preDiv [] _ = Left("Div function needs at least two arguments")
    preDiv (x:y:xs) env = case length (x:y:xs) of
        2 -> case checkIfEmpty (x:y:xs) of
            True -> case checkFloatInt (x:y:xs) env of
                Right True -> case checkZero (y) env of
                    Right _ -> (division (convertIntFloat[x, y]) (env))
                    Left err -> Left err
                Right False -> Left ("There is at least one float" ++ show (x:y:xs))
                Left err -> Left err
            False -> Left ("There is at least one symbol that isn't defined" ++ show (x:y:xs))
        _ -> Left "Div function only needs two arguments"
    preDiv _ _ = Left "Div function only needs two arguments"

    division :: [Ast] -> Env -> Either String Ast
    division [] _ = Right (FloatAst 1)
    division list _ = Right (FloatAst (foldl1 (/) [x | FloatAst x <- list]))

    preMod :: [Ast] -> Env -> Either String Ast
    preMod [] _ = Left "Mod function needs at least two arguments"
    preMod (x:y:xs) env = case length (x:y:xs) of
        2 -> case checkIfEmpty(convertFloatIntList (x:y:xs)) of
            True -> case (checkZero (convertFloatInt y) env) of
                Right _ -> modulo (convertFloatIntList(x:y:xs)) env
                Left err -> Left err
            False -> Left ("There is at least one parameter that isn't an integer")
        _ -> Left "Mod function only needs two arguments"
    preMod _ _ = Left "Mod function only needs two arguments"

    modulo :: [Ast] -> Env -> Either String Ast
    modulo (IntegerAst x: IntegerAst y: _) _ = Right (IntegerAst (x `mod` y))
    modulo _ _ = Left "Not a number"

    preSup :: [Ast] -> Env -> Either String Ast
    preSup [] _ = Left "> operator needs at least two arguments"
    preSup args env = case length args of
        2 -> case checkIfEmpty args of
            True -> sup (convertIntFloat(args)) env
            False -> Left ("There is at least one symbol that isn't defined" ++ show args)
        _ -> Left "> operator needs at least two arguments"

    sup :: [Ast] -> Env -> Either String Ast
    sup [] _ = Left "> operator needs at least two arguments"
    sup (IntegerAst x : IntegerAst y : []) _ = case x > y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    sup (FloatAst x : FloatAst y : []) _ = case x > y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    sup _ _ = Left "Not a number"
    
    preInf :: [Ast] -> Env -> Either String Ast
    preInf [] _ = Left "< operator needs at least two arguments"
    preInf args env = case length args of
        2 -> case checkIfEmpty args of
            True -> inf (convertIntFloat(args)) env
            False -> Left ("There is at least one symbol that isn't defined" ++ show args)
        _ -> Left "< operator needs at least two arguments"
    
    inf :: [Ast] -> Env -> Either String Ast
    inf [] _ = Left "< operator needs at least two arguments"
    inf (IntegerAst x : IntegerAst y : []) _ = case x < y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    inf (FloatAst x : FloatAst y : []) _ = case x < y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    inf ast env = error (show ast ++ show env)
