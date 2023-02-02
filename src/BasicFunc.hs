module BasicFunc (
    pre_add,
    pre_sub,
    mul,
    division,
    modulo,
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
        _ -> Left "Not a number"


    --sub :: [Ast] -> Ast
    --sub [] = error "Empty list"
    -- sub (Call x : xs) = sub ((evalAst (Call x)) : xs)
    --sub (SymbolAst x : xs) = error "Not a number"
    --sub (Define x y : xs) = error "Not a number"
    --sub list = IntegerAst (foldl1 (-) [x | IntegerAst x <- list])

    mul :: Float -> Float -> Ast
    mul x y = (FloatAst (x * y))

    --mul :: [Ast] -> Ast
    --mul [] = error "Empty list"
    -- mul (Call x : xs) = mul ((evalAst (Call x)) : xs)
    --mul (SymbolAst x : xs) = error "Not a number"
    --mul (Define x y : xs) = error "Not a number"
    --mul list = IntegerAst (product [x | IntegerAst x <- list])

    division :: Float -> Float -> Ast
    division x y = FloatAst (x / y)

    modulo :: Int -> Int -> Ast
    modulo x y = IntegerAst (x `mod` y)

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