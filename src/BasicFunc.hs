module BasicFunc (
    check_float_int,
    pre_add,
    pre_sub,
    pre_mul,
    pre_div,
    pre_mod,
    fact
) where

    import Info

    check_float_int :: [Ast] -> Env -> Either String Bool
    check_float_int [] env = Right True
    check_float_int (x:xs) env = case x of
        (IntegerAst i) -> check_float_int xs env
        (FloatAst f) -> check_float_int xs env
        (SymbolAst s) -> case getValueEnv env s of
            Right ast -> case ast of
                (IntegerAst i) -> check_float_int xs env
                (FloatAst f) -> check_float_int xs env
                _ -> Left "The symbol \'s\' isn't valid"
            Left err -> Left err
        _ -> Left "The arg \'x\' is not a number"
    
    pre_add :: [Ast] -> Env -> Either String Ast
    pre_add [] env = Left "Add function needs at least two arguments"
    pre_add args env = case check_float_int args env of
        Right x -> add args env
        Left err -> Left err

    add :: [Ast] -> Env -> Either String Ast
    add [] env = Right (IntegerAst 0)
    add (x:xs) env = case x of
        (IntegerAst i) -> Right (IntegerAst (i + (sum [x | IntegerAst x <- xs])))
        (FloatAst f) -> Right (FloatAst (f + (sum [x | FloatAst x <- xs])))
        (SymbolAst s) -> case getValueEnv env s of
            Right ast -> case ast of
                (IntegerAst i) -> Right (IntegerAst (i + (sum [x | IntegerAst x <- xs])))
                (FloatAst f) -> Right (FloatAst (f + (sum [x | FloatAst x <- xs])))
                _ -> Left "The symbol \'s\' isn't valid"
            Left err -> Left err

    --add [] = error "Empty list"
    -- add (Call x : xs) = add ((evalAst (Call x)) : xs)
    --add (SymbolAst x : xs) = error "Not a number"
    --add (Define x y : xs) = error "Not a number"
    --add list = IntegerAst (sum [x |  IntegerAst x <- list])


    pre_sub :: [Ast] -> Env -> Either String Ast
    pre_sub [] env = Left "Sub function needs at least two arguments"
    pre_sub args env = case check_float_int args env of
        Right x -> sub args env
        Left err -> Left err

    sub :: [Ast] -> Env -> Either String Ast
    sub [] env = Right (IntegerAst 0)
    sub (x:xs) env = case x of
        (IntegerAst i) -> Right (IntegerAst (i - (sum [x | IntegerAst x <- xs])))
        (FloatAst f) -> Right (FloatAst (f - (sum [x | FloatAst x <- xs])))
        (SymbolAst s) -> case getValueEnv env s of
            Right ast -> case ast of
                (IntegerAst i) -> Right (IntegerAst (i - (sum [x | IntegerAst x <- xs])))
                (FloatAst f) -> Right (FloatAst (f - (sum [x | FloatAst x <- xs])))
                _ -> Left "The symbol \'s\' isn't valid"
            Left err -> Left err
        _ -> Left "Not a number"


    --sub :: [Ast] -> Ast
    --sub [] = error "Empty list"
    -- sub (Call x : xs) = sub ((evalAst (Call x)) : xs)
    --sub (SymbolAst x : xs) = error "Not a number"
    --sub (Define x y : xs) = error "Not a number"
    --sub list = IntegerAst (foldl1 (-) [x | IntegerAst x <- list])

    pre_mul :: [Ast] -> Env -> Either String Ast
    pre_mul [] env = Left "Mul function needs at least two arguments"
    pre_mul args env = case check_float_int args env of
        Right x -> mul args env
        Left err -> Left err

    mul :: [Ast] -> Env -> Either String Ast
    mul [] env = Right (IntegerAst 1)
    mul (x:xs) env = case x of
        (IntegerAst i) -> Right (IntegerAst (i * (product [x | IntegerAst x <- xs])))
        (FloatAst f) -> Right (FloatAst (f * (product [x | FloatAst x <- xs])))
        (SymbolAst l) -> case getValueEnv env l of
            Right ast -> case ast of
                (IntegerAst i) -> Right (IntegerAst (i * (product [x | IntegerAst x <- xs])))
                (FloatAst f) -> Right (FloatAst (f * (product [x | FloatAst x <- xs])))
                _ -> Left "The symbol \'l\' isn't valid"
            Left err -> Left err

    --mul :: [Ast] -> Ast
    --mul [] = error "Empty list"
    -- mul (Call x : xs) = mul ((evalAst (Call x)) : xs)
    --mul (SymbolAst x : xs) = error "Not a number"
    --mul (Define x y : xs) = error "Not a number"
    --mul list = IntegerAst (product [x | IntegerAst x <- list])


    check_zero :: Ast -> Env -> Either String Ast
    check_zero (IntegerAst i) env = case i of
        0 -> Left "Second argument is null"
        _ -> Right (IntegerAst i)
    check_zero (FloatAst f) env = case f of
        0 -> Left "Second argument is null"
        _ -> Right (FloatAst f)
    check_zero (SymbolAst s) env = case getValueEnv env s of
        Right ast -> case ast of
            (IntegerAst i) -> case i of
                0 -> Left "Second argument is null"
                _ -> Right (IntegerAst i)
            (FloatAst f) -> case f of
                0 -> Left "Second argument is null"
                _ -> Right (FloatAst f)
            _ -> Left "The symbol \'s\' isn't valid"
        Left err -> Left err
    check_zero _ env = Left "Not a number"

    pre_div :: [Ast] -> Env -> Either String Ast
    pre_div [] env = Left "Div function needs at least two arguments"
    -- no more than two arguments
    pre_div (x:y:xs) env = case length xs of
        0 -> case check_float_int [x,y] env of
            Right res -> case check_zero y env of
                Right res -> division (x:y:xs) env
                Left err -> Left err
            Left err -> Left err
        _ -> Left "Div function only needs two arguments"

    division :: [Ast] -> Env -> Either String Ast
    division [] env = Right (IntegerAst 1)
    division list env = Right (IntegerAst (foldl1 div [x | IntegerAst x <- list]))


    check_only_int :: [Ast] -> Env -> Bool
    check_only_int [] env = True
    check_only_int (x:xs) env = case x of
        (IntegerAst i) -> check_only_int xs env
        (SymbolAst s) -> case getValueEnv env s of
            Right ast -> case ast of
                (IntegerAst i) -> check_only_int xs env
                _ -> False
            Left err -> False
        _ -> False

    pre_mod :: [Ast] -> Env -> Either String Ast
    pre_mod [] env = Left "Mod function needs at least two arguments"
    pre_mod (x:y:xs) env = case length (x:y:xs) of
        2 -> case check_only_int (x:y:xs) env of
            True -> case check_zero y env of
                Right res -> Right (modulo x y env)
                Left err -> Left err
            False -> Left "Mod function only works with integers"
        _ -> Left "Mod function only needs two arguments"

    modulo :: Ast -> Ast -> Env -> Ast
    modulo (IntegerAst x) (IntegerAst y) env = IntegerAst (x `mod` y)
    modulo (IntegerAst x) (SymbolAst y) env = case getValueEnv env y of
        Right ast -> case ast of
            (IntegerAst i) -> IntegerAst (x `mod` i)
            _ -> error "Not a number"
        Left err -> error err
    modulo (SymbolAst x) (IntegerAst y) env = case getValueEnv env x of
        Right ast -> case ast of
            (IntegerAst i) -> IntegerAst (i `mod` y)
            _ -> error "Not a number"
        Left err -> error err
    modulo (SymbolAst x) (SymbolAst y) env = case getValueEnv env x of
        Right ast -> case ast of
            (IntegerAst i) -> case getValueEnv env y of
                Right ast -> case ast of
                    (IntegerAst j) -> IntegerAst (i `mod` j)
                    _ -> error "Not a number"
                Left err -> error err
            _ -> error "Not a number"
        Left err -> error err
    modulo _ _ _ = error "Not a number"

    fact :: Int -> Ast
    fact x = IntegerAst (product [1..x])

    --div :: [Ast] -> Ast
    --div [] = error "Empty list"
    -- div (Call x : xs) = div ((evalAst (Call x)) : xs)
    --div (SymbolAst x : xs) = error "Not a number"
    --div (Define x y : xs) = error "Not a number"
    --div list = IntegerAst (foldl1 div [x | IntegerAst x <- list])

    --mod :: [Ast] -> Ast
    --mod [] = error "Empty list"
    -- mod (Call x : xs) = mod ((evalAst (Call x)) : xs)
    --mod (SymbolAst x : xs) = error "Not a number"
    --mod (Define x y : xs) = error "Not a number"
    --mod list = IntegerAst (foldl1 mod [x | IntegerAst x <- list])