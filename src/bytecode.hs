module Bytecode (

) where

    import Info

    getToken :: String -> Maybe String
    getToken "+" = Just "BINARY_ADD"
    getToken "-" = Just "BINARY_SUBTRACT"
    getToken "*" = Just "BINARY_MULTIPLY"
    getToken "/" = Just "BINARY_DIVIDE"
    getToken ">" = Just "BINARY_GT"
    getToken "<" = Just "BINARY_LT"
    getToken "==" = Just "BINARY_EQ"
    getToken "eq?" = Just "BINARY_EQ"
    getToken "%" = Just "BINARY_MODULO"
    getToken _ = Nothing


    createByteCode :: Ast -> Env -> Stack -> Stack
    createByteCode _ _ stack = stack
    createByteCode (Define name ast) env stack = case ast of
        SymbolASt s -> case getToken s of
            Just token -> stack { bytecode = bytecode + ["LOAD_CONST " ++ (show (length (const_value))), "STORE_FAST " ++ (show (length (fast)))], const_value ++ [token], fast ++ [(name, ast)] }
            Nothing -> error "The token isn't valid"