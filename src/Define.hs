module Define (
    defineFunc,
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
    defineFunc name value env = case isInEnv name env of
        True -> Right (Empty, replaceInEnv name value env)
        False -> Right (Empty, updateEnv name value env)