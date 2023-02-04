module Keywords (
    preEqFunc,
) where
    import Info
    import Data.Eq

    preEqFunc :: [Ast] -> Env -> Either String Ast
    preEqFunc [] env = Left "If function needs at least three arguments"
    preEqFunc args env = case length args of
        2 -> case checkIfEmpty (convertArgs args env) of
            True -> eqFunc (convertArgs args env) env
            False -> Left "There is at least a symbol that doesn't exist"
        _ -> Left "If function needs at least three arguments"

    
    eqFunc :: [Ast] -> Env -> Either String Ast
    eqFunc [] env = Left "If function needs at least three arguments"
    eqFunc (IntegerAst x : IntegerAst y : []) env = case x == y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")
    eqFunc (FloatAst x : FloatAst y : []) env = case x == y of
        True -> Right (SymbolAst "#t")
        False -> Right (SymbolAst "#f")