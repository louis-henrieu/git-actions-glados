module BasicFunc (
    add,
    sub,
    mul,
    division,
    modulo,
    fact
) where

    import Info

    add :: Float -> Float -> Ast
    add x y = (FloatAst(x + y))

    --add [] = error "Empty list"
    -- add (Call x : xs) = add ((evalAst (Call x)) : xs)
    --add (SymbolAst x : xs) = error "Not a number"
    --add (Define x y : xs) = error "Not a number"
    --add list = IntegerAst (sum [x |  IntegerAst x <- list])

    sub :: Float -> Float -> Ast
    sub x y = (FloatAst(x - y))

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