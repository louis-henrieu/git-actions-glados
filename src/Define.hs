module Define (
    defineFunc,
    isInEnv,
) where

    import Info
    import Env

    isInEnv :: String -> [(String, Ast)] -> Bool
    isInEnv _ [] = False
    isInEnv name ((key, _):xs) = if name == key then True else isInEnv name xs

    replaceInEnv :: String -> Ast -> [(String, Ast)] -> [(String, Ast)]
    replaceInEnv _ _ [] = []
    replaceInEnv name value ((key, ast):xs) = 
        if name == key 
            then 
                (key, value) : xs 
            else 
                (key, ast) : replaceInEnv name value xs

    defineFunc :: String -> Ast -> Env ->  Either String (Ast, Env)
    defineFunc name value env = case value of
        (Lambda x y) -> case isInEnv name env of
            False -> Right (Empty, (name, ArgsLambda (x, y)):env)
            True -> Right (Empty, replaceInEnv name (ArgsLambda (x, y)) env)

        (SymbolAst x) -> case getValueEnv env x of
            Left err -> Left err
            Right ast -> case isInEnv name env of
                False -> Right (Empty, (name, ast):env)
                True -> Right (Empty, replaceInEnv name ast env)
        _ -> case isInEnv name env of
            False -> Right (Empty, (name, value):env)
            True -> Right (Empty, replaceInEnv name value env)