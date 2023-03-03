module Bytecode (
    createByteCode
) where

    import Info
    import Ast

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

    addByteCodeConst :: Stack -> Maybe Stack
    addByteCodeConst stack = 
        Just stack { bytecode = bytecode stack ++ ["LOAD_CONST " ++ (show (length (constValue stack) + 1)), "STORE_FAST " ++ (show (length (fast stack))), ""], end = False }
    
    addFast :: Stack -> String -> Maybe Stack
    addFast stack name = 
        Just stack { fast = fast stack ++ [name] }
    
    addConst :: Stack -> Ast -> Maybe Stack
    addConst stack token = 
        Just stack { constValue = constValue stack ++ [token] }


    createByteCode :: Ast -> Env -> Stack -> Stack
    createByteCode (Define name ast) env stack = case ast of
        SymbolAst s -> error s
        FloatAst f -> case addByteCodeConst stack of
            Just s2 -> case addFast s2 name of
                Just s3 -> case addConst s3 (FloatAst f) of
                    Just s4 -> s4
                    Nothing -> error "Not implemented yet"
        IntegerAst i -> case addByteCodeConst stack of
            Just s2 -> case addFast s2 name of
                Just s3 -> case addConst s3 (IntegerAst i) of
                    Just s4 -> s4
                    Nothing -> error "Not implemented yet"
        _ -> error "Not implemented yet"
    createByteCode _ _ stack = stack { bytecode = bytecode stack ++ ["LOAD_CONST 0", "RETURN_VALUE"], end = True }

    --callAdd :: 