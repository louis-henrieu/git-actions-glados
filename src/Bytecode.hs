module Bytecode (
    createByteCode
) where

    import Info
    import Ast

    comparators = ["<", ">", "<=", ">=", "==", "!="]

    getToken :: String -> Maybe String
    getToken "+" = Just "BINARY_ADD"
    getToken "-" = Just "BINARY_SUBTRACT"
    getToken "*" = Just "BINARY_MULTIPLY"
    getToken "/" = Just "BINARY_DIVIDE"
    getToken "%" = Just "BINARY_MODULO"
    getToken _ = Nothing
     
    addFast :: Stack -> String -> Maybe Stack
    addFast stack name = 
        Just stack { bytecode = bytecode stack ++ ["STORE_FAST " ++ (show (length (fast stack)))], fast = fast stack ++ [name] }

    isEqual :: Ast -> Ast -> Bool
    isEqual (SymbolAst s1) (SymbolAst s2) = s1 == s2
    isEqual (IntegerAst i1) (IntegerAst i2) = i1 == i2
    isEqual (FloatAst f1) (FloatAst f2) = f1 == f2
    isEqual _ _ = False


    getIndexListA :: Ast -> [Ast] -> Int -> Maybe Int
    getIndexListA _ [] _ = Nothing
    getIndexListA s (x : xs) i = if (isEqual s x)
        then Just i
        else getIndexListA s xs (i + 1)
 
    getIndexListI :: String -> [String] -> Int -> Maybe Int
    getIndexListI _ [] _ = Nothing
    getIndexListI s (x : xs) i = if (s == x)
        then Just i
        else getIndexListI s xs (i + 1)

    loadFast :: Stack -> String -> Maybe Stack
    loadFast stack name = case getIndexListI name (fast stack) 0 of
        Just i -> Just stack { bytecode = bytecode stack ++ ["LOAD_FAST " ++ (show i)] }
        Nothing -> Just stack { bytecode = bytecode stack ++ ["STORE_FAST " ++ (show (length (fast stack)))], fast = fast stack ++ [name] }

    addConst :: Stack -> Ast -> Maybe Stack
    addConst stack token = 
        Just stack { bytecode = bytecode stack ++ ["LOAD_CONST " ++ (show (length (constValue stack) + 1))], constValue = constValue stack ++ [token] }
    
    loadConst :: Stack -> Ast -> Maybe Stack
    loadConst stack token = case getIndexListA token (constValue stack) 0 of
        Just i -> Just stack { bytecode = bytecode stack ++ ["LOAD_CONST " ++ (show i)] }
        Nothing -> Just stack { bytecode = bytecode stack ++ ["LOAD_CONST " ++ (show (length (constValue stack) + 1))], constValue = constValue stack ++ [token] }

    addGlobal :: Stack -> String -> Maybe Stack
    addGlobal stack name = 
        Just stack { bytecode = bytecode stack ++ ["LOAD_GLOBAL " ++ (show (length (global stack)))], global = global stack ++ [name] }

    loadGlobal :: Stack -> String -> Maybe Stack
    loadGlobal stack name = case getIndexListI name (global stack) 0 of
        Just i -> Just stack { bytecode = bytecode stack ++ ["LOAD_GLOBAL " ++ (show i)] }
        Nothing -> Just stack { bytecode = bytecode stack ++ ["LOAD_GLOBAL " ++ (show (length (global stack)))], global = global stack ++ [name] }

    endByteCode :: Stack -> Stack
    endByteCode stack = stack { bytecode = bytecode stack ++ ["LOAD_CONST 0", "RETURN_VALUE"], end = True }

    checkSymbols :: [Ast] -> Bool
    checkSymbols [] = False
    checkSymbols (x : xs) = case x of
        SymbolAst s -> True
        Call (y : ys) -> checkSymbols ys
        _ -> checkSymbols xs

    addFunction :: Stack -> String -> Maybe Stack
    addFunction stack name = case getToken name of
        Just s -> Just stack
        Nothing -> case loadGlobal stack name of
            Just s2 -> Just s2
            Nothing -> error "Should not be happening 4"

    createCallByteCode :: [Ast] -> Env -> Stack -> Stack
    createCallByteCode [] _ stack = stack
    createCallByteCode (x : xs) env stack = case x of
        Call (SymbolAst y : ys) -> case addFunction stack y of
            Just s2 -> createCallByteCode ys env s2
            Nothing -> error "Not implemented yet"
        _ -> createCallByteCode xs env (createAstByteCode x env stack)

    checkList :: a -> [a] -> Bool
    checkList _ [] = False
    checkList s (x : xs) = case s of
        x -> True
        _ -> checkList s xs

    createAstByteCode :: Ast -> Env -> Stack -> Stack
    createAstByteCode (SymbolAst s) env stack = case checkList s (fast stack) of
        True -> case loadFast stack s of
            Just new_stack -> new_stack
            Nothing -> error "Should not be happening 1"
        False -> case (loadGlobal stack s) of
            Just new_stack -> new_stack
            Nothing -> error "Should not be happening 2"
    createAstByteCode (FloatAst f) env stack = case (checkList (FloatAst f) (constValue stack)) of
        True -> case loadConst stack (FloatAst f) of
            Just s2 -> s2
            _ -> error "Should not be happening 3"
        False -> case addConst stack (FloatAst f) of
            Just s2 -> s2
            _ -> error "Should not be happening 4"
    createAstByteCode (IntegerAst i) env stack = case (checkList (IntegerAst i) (constValue stack)) of
        True -> case loadConst stack (IntegerAst i) of
            Just s2 -> s2
        False -> case addConst stack (IntegerAst i) of
            Just s2 -> s2
    createAstByteCode _ _ stack = endByteCode stack

    preCreateCallByteCode :: [Ast] -> Env -> Stack -> Maybe Stack
    preCreateCallByteCode [] _ stack = Just stack
    preCreateCallByteCode list env stack = Just (createCallByteCode list env stack)

    addFormulas :: Stack -> [Ast] -> Maybe Stack
    addFormulas stack [] = Just stack
    addFormulas stack (x : xs) = case x of
        Call (SymbolAst y : ys) -> case (getToken y) of
            Just s -> addFormulas stack{ bytecode = bytecode stack ++ [s] } xs
            Nothing -> addFormulas stack{ bytecode = bytecode stack ++ ["CALL_FUNCTION " ++ (show (length ys))] } xs
        _ -> addFormulas stack xs



    createByteCode :: Ast -> Env -> Stack -> Stack
    createByteCode (Define name ast) env stack = case ast of
        SymbolAst s -> case checkList s (fast stack) of
            True -> case loadFast stack s of
                Just new_stack -> new_stack
                Nothing -> error "Should not be happening"
            False -> case (checkList s (global stack)) of
                True -> case loadGlobal stack s of
                    Just new_stack -> new_stack
                    Nothing -> error "Should not be happening"
                False -> case addGlobal stack s of
                    Just new_stack -> new_stack
                    Nothing -> error "Should not be happening"       
        FloatAst f -> case (checkList (FloatAst f) (constValue stack)) of
            True -> case loadConst stack (FloatAst f) of
                Just s2 -> case addFast stack name of
                    Just s3 -> s3
                    Nothing -> error "Not implemented yet"
            False -> case addConst stack (FloatAst f) of
                Just s2 -> case addFast stack name of
                    Just s3 -> s3
                    Nothing -> error "Not implemented yet"
        IntegerAst i -> case (checkList (IntegerAst i) (constValue stack)) of
            True -> case loadConst stack (IntegerAst i) of
                Just s2 -> case addFast stack name of
                    Just s3 -> s3
                    Nothing -> error "Not implemented yet"
            False -> case addConst stack (IntegerAst i) of
                Just s2 -> case addFast stack name of
                    Just s3 -> s3
                    Nothing -> error "Not implemented yet"
        Call (SymbolAst x : xs) -> case addFunction stack x of
            Just new_stack -> case preCreateCallByteCode xs env new_stack of
                Just post_stack -> case addFormulas post_stack xs of
                    Just final_stack -> case getToken x of
                        Just s -> case addFast (final_stack { bytecode = bytecode final_stack ++ [s] }) name of
                            Just s2 -> s2
                            Nothing -> error "Not implemented yet"
                        Nothing -> case addFast (final_stack { bytecode = bytecode final_stack ++ ["CALL_FUNCTION " ++ (show (length xs))] }) name of
                            Just s2 -> s2
                            Nothing -> error "Not implemented yet"
                    Nothing -> error "Not implemented yet"
    createByteCode (Call (x : xs)) env stack = case checkSymbols xs of
        True -> error "Not implemented yet"
        False -> endByteCode stack
    createByteCode _ _ stack = endByteCode stack

    --callAdd :: 