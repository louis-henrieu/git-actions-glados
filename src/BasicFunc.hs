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

    convertIntFloat :: [Ast] -> [Ast]
    convertIntFloat [] = []
    convertIntFloat (x:xs) = case x of
        (IntegerAst x) -> FloatAst (fromIntegral x) : convertIntFloat xs
        ast -> ast : convertIntFloat xs

    checkFloatInt :: [Ast] -> Env -> Either String Bool
    checkFloatInt [] env = Right True
    checkFloatInt (x:xs) env = case x of
        (IntegerAst i) -> checkFloatInt xs env
        (FloatAst f) -> checkFloatInt xs env
        _ -> Left "The arg \'x\' is not a number"
    
    preAdd :: [Ast] -> Env -> Either String Ast
    preAdd [] env = Left "Add function needs at least two arguments"
    preAdd args env = case checkIfEmpty args of
        True -> case checkFloatInt args env of
            Right True -> add (convertIntFloat(args)) env
            Left err -> Left err
        False -> Left ("There is at least one symbol that isn't defined" ++ show args)

    add :: [Ast] -> Env -> Either String Ast
    add [] env = Right (IntegerAst 0)
    add (x:xs) env = case x of
        (IntegerAst i) -> Right (IntegerAst (i + (sum [x | IntegerAst x <- xs])))
        (FloatAst f) -> Right (FloatAst (f + (sum [x | FloatAst x <- xs])))
        _ -> Left "Not a number"

    preSub :: [Ast] -> Env -> Either String Ast
    preSub [] env = Left "Sub function needs at least two arguments"
    preSub args env = case checkIfEmpty args  of
        True -> case checkFloatInt args env of
            Right True -> sub (convertIntFloat(args)) env
            Left err -> Left err
        False -> Left ("There is at least one symbol that isn't defined" ++ show args)

    sub :: [Ast] -> Env -> Either String Ast
    sub [] env = Right (IntegerAst 0)
    sub (x:xs) env = case x of
        (IntegerAst i) -> Right (IntegerAst (i - (sum [x | IntegerAst x <- xs])))
        (FloatAst f) -> Right (FloatAst (f - (sum [x | FloatAst x <- xs])))
        _ -> Left "Not a number"

    preMul :: [Ast] -> Env -> Either String Ast
    preMul [] env = Left "Mul function needs at least two arguments"
    preMul args env = case checkIfEmpty args of
        True -> case checkFloatInt args env of
            Right True -> mul (convertIntFloat(args)) env
            Left err -> Left err
        False -> Left ("There is at least one symbol that isn't defined" ++ show args)

    mul :: [Ast] -> Env -> Either String Ast
    mul [] env = Right (IntegerAst 1)
    mul (x:xs) env = case x of
        (IntegerAst i) -> Right (IntegerAst (i * (product [x | IntegerAst x <- xs])))
        (FloatAst f) -> Right (FloatAst (f * (product [x | FloatAst x <- xs])))
        _ -> Left "Not a number"

    checkZero :: Ast -> Env -> Either String Ast
    checkZero (IntegerAst i) env = case i of
        0 -> Left "Second argument is not allowed to be a 0"
        _ -> Right (IntegerAst i)
    checkZero (FloatAst f) env = case f of
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
    checkZero _ env = Left "Not a number"

    preDiv :: [Ast] -> Env -> Either String Ast
    preDiv [] env = Left "Div function needs at least two arguments"
    -- no more than two arguments
    preDiv (x:y:xs) env = case length (x:y:xs) of
        2 -> case checkIfEmpty (x:y:xs) of
            True -> case checkFloatInt (x:y:xs) env of
                Right True -> case checkZero (y) env of
                    Right ast -> (division (convertIntFloat[x, y]) (env))
                    Left err -> Left err
                Left err -> Left err
            False -> Left ("There is at least one symbol that isn't defined" ++ show (x:y:xs))
        _ -> Left "Div function only needs two arguments"

    division :: [Ast] -> Env -> Either String Ast
    division [] env = Right (FloatAst 1)
    division list env = Right (FloatAst (foldl1 (/) [x | FloatAst x <- list]))


    convertFloatInt :: Ast -> Ast
    convertFloatInt (FloatAst f) = if f == (fromIntegral (round f)) then 
            IntegerAst (round (f)) 
        else 
            Empty
    convertFloatInt _ = Empty

    convertFloatIntList :: [Ast] -> [Ast]
    convertFloatIntList [] = []
    convertFloatIntList (x:xs) = convertFloatInt (x) : convertFloatIntList xs

    preMod :: [Ast] -> Env -> Either String Ast
    preMod [] env = Left "Mod function needs at least two arguments"
    preMod (x:y:xs) env = case length (x:y:xs) of
        2 -> case checkIfEmpty(convertFloatIntList (x:y:xs)) of
            True -> case (checkZero (convertFloatInt y) env) of
                Right ast -> modulo (convertFloatIntList(x:y:xs)) env
                Left err -> Left err
            False -> Left ("There is at least one parameter that isn't an integer")
        _ -> Left "Mod function only needs two arguments"

    modulo :: [Ast] -> Env -> Either String Ast
    modulo (IntegerAst x: IntegerAst y: xs) env = Right (IntegerAst (x `mod` y))
    modulo _ _ = Left "Not a number"

    preSup :: [Ast] -> Env -> Either String Ast
    preSup [] env = Left "> operator needs at least two arguments"
    preSup args env = case length args of
        2 -> case checkIfEmpty args of
            True -> sup (convertIntFloat(args)) env
            False -> Left ("There is at least one symbol that isn't defined" ++ show args)
        _ -> Left "> operator needs at least two arguments"

    sup :: [Ast] -> Env -> Either String Ast
    sup [] env = Left "> operator needs at least two arguments"
    sup (IntegerAst x : IntegerAst y : []) env = case x > y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    sup (FloatAst x : FloatAst y : []) env = case x > y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    
    preInf :: [Ast] -> Env -> Either String Ast
    preInf [] env = Left "< operator needs at least two arguments"
    preInf args env = case length args of
        2 -> case checkIfEmpty args of
            True -> inf (convertIntFloat(args)) env
            False -> Left ("There is at least one symbol that isn't defined" ++ show args)
        _ -> Left "< operator needs at least two arguments"
    
    inf :: [Ast] -> Env -> Either String Ast
    inf [] env = Left "< operator needs at least two arguments"
    inf (IntegerAst x : IntegerAst y : []) env = case x < y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    inf (FloatAst x : FloatAst y : []) env = case x < y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
