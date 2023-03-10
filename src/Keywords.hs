module Keywords (
    preEqFunc,
    eqFunc
) where
    import Info

    preEqFunc :: [Ast] -> Env -> Either String Ast
    preEqFunc [] _ = Left "If function needs at least three arguments"
    preEqFunc args env = case length args of
        2 -> case checkIfEmpty args of
            True -> eqFunc args env
            False -> Left ("There is at least one symbol that isn't defined" ++ show args)
        _ -> Left "If function needs at least three arguments"

    argsToString :: [Ast] -> String
    argsToString [] = ""
    argsToString (SymbolAst x : xs) = x ++ " || " ++ argsToString xs
    argsToString (x : xs) = show x ++ " || " ++ argsToString xs 

    eqFunc :: [Ast] -> Env -> Either String Ast
    eqFunc [] _ = Left "If function needs at least three arguments"
    eqFunc (IntegerAst x : IntegerAst y : []) _ = case x == y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    eqFunc (FloatAst x : FloatAst y : []) _ = case x == y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    eqFunc args _ = Left ("args are not equal: " ++ (argsToString args))