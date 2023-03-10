module Bytecode (
    createByteCode
) where

    import Info
    import Ast

    comparators = ["<", ">", "<=", ">=", "==", "!="]

    addByteCode :: Stack -> [String] -> Stack
    addByteCode stack [] = stack
    addByteCode stack (initList : list) = case length (bytecode stack) of
        0 -> error "Should not be happening"
        1 -> addByteCode (stack { bytecode = [(last (bytecode stack)) ++ initList, ""] }) list
        _ -> addByteCode (stack { bytecode = (init (bytecode stack)) ++ [(last (bytecode stack)) ++ initList, ""] }) list

    getToken :: String -> Maybe String
    getToken "+" = Just "BINARY_ADD"
    getToken "-" = Just "BINARY_SUBTRACT"
    getToken "*" = Just "BINARY_MULTIPLY"
    getToken "/" = Just "BINARY_DIVIDE"
    getToken "%" = Just "BINARY_MODULO"
    getToken token = if token `elem` comparators
                    then Just "COMPARE_OP"
                    else Nothing
    getToken _ = Nothing

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
        Just i -> Just (addByteCode stack ["\t" ++ (show (dualNum stack)) ++ "\tLOAD_FAST " ++ (show i) ++ "\t\t(" ++ name ++ ")"])
        Nothing -> Just (addByteCode (stack { fast = fast stack ++ [name], dualNum = dualNum stack + 2})  ["\t" ++ (show (dualNum stack)) ++ "\tSTORE_FAST " ++ (show (length (fast stack))) ++ "\t\t(" ++ name ++ ")"])

    displayToken :: Ast -> String
    displayToken (IntegerAst i) = show i
    displayToken (FloatAst f) = if (f == fromIntegral (round f :: Integer)) then show (fromIntegral (round f :: Integer)) else show f
    displayToken _ = error "Should not be happening"

    loadConst :: Stack -> Ast -> Maybe Stack
    loadConst stack token = case getIndexListA token (constValue stack) 0 of
        Just i -> Just (addByteCode (stack {dualNum = dualNum stack + 2}) ["\t" ++ (show (dualNum stack)) ++ "\tLOAD_CONST " ++ (show i) ++ "\t\t(" ++ (displayToken token) ++ ")"])
        Nothing -> Just (addByteCode (stack { constValue = constValue stack ++ [token], dualNum = dualNum stack + 2}) ["\t" ++ (show (dualNum stack)) ++ "\tLOAD_CONST " ++ (show (length (constValue stack))) ++ "\t\t(" ++ (displayToken token) ++ ")"])

    loadGlobal :: Stack -> String -> Maybe Stack
    loadGlobal stack name = case getIndexListI name (global stack) 0 of
        Just i -> Just (addByteCode (stack { dualNum = dualNum stack + 2 }) ["\t" ++ (show (dualNum stack)) ++ "\tLOAD_GLOBAL " ++ (show i) ++ "\t(" ++ name ++ ")"])
        Nothing -> Just (addByteCode (stack { global = global stack ++ [name], dualNum = dualNum stack + 4 }) ["\t" ++ (show (dualNum stack)) ++ "\tSTORE_GLOBAL " ++ (show (length (global stack))) ++ "\t\t(" ++ name ++ ")", "\t" ++ (show (dualNum stack + 2)) ++ "\tLOAD_GLOBAL " ++ (show (length (global stack))) ++ "\t\t(" ++ name ++ ")"])
    
    compareOp :: Stack -> String -> Maybe Stack
    compareOp stack op = case getIndexListI op comparators 0 of
        Just i -> Just (addByteCode (stack { dualNum = dualNum stack + 2 }) ["\t" ++ (show (dualNum stack)) ++ "\tCOMPARE_OP " ++ (show i) ++ "\t\t(" ++ op ++ ")"])
        Nothing -> Nothing

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
        st -> True
        _ -> checkList s xs

    createAstByteCode :: Ast -> Env -> Stack -> Stack
    createAstByteCode (SymbolAst s) env stack = case checkList s (fast stack) of
        True -> case loadFast stack s of
            Just newStack -> newStack
            Nothing -> error "Should not be happening 1"
        False -> case (loadGlobal stack s) of
            Just newStack -> newStack
            Nothing -> error "Should not be happening 2"
    createAstByteCode (FloatAst f) env stack = case loadConst stack (FloatAst f) of
        Just s2 -> s2
        _ -> error "Should not be happening 3"
    createAstByteCode (IntegerAst i) env stack = case loadConst stack (IntegerAst i) of
        Just s2 -> s2
        _ -> error "Should not be happening 5"
    createAstByteCode _ _ stack = addByteCode stack ["LOAD_CONST 0\t\t(None)", "RETURN_VALUE"]

    preCreateCallByteCode :: [Ast] -> Env -> Stack -> Maybe Stack
    preCreateCallByteCode [] _ stack = Just stack
    preCreateCallByteCode list env stack = Just (createCallByteCode list env stack)

    addFormulas :: Stack -> [Ast] -> Maybe Stack
    addFormulas stack [] = Just stack
    addFormulas stack (x : xs) = case x of
        Call (SymbolAst y : ys) -> case (getToken y) of
            Just s -> addFormulas stack { bytecode = (init (bytecode stack)) ++ ["\t" ++ (show (dualNum stack)) ++ "\t" ++ (last (bytecode stack)) ++ s, ""], dualNum = dualNum stack + 2 } xs
            Nothing -> addFormulas stack { bytecode = (init (bytecode stack)) ++ ["\t" ++ (show (dualNum stack)) ++ "\t" ++ (last (bytecode stack)) ++ "CALL_FUNCTION " ++ (show (length ys)), ""], dualNum = dualNum stack + 2 } xs
        _ -> addFormulas stack xs

    callFunctionByteCode :: Stack -> Int -> Stack
    callFunctionByteCode stack nb_args = addByteCode (stack {dualNum = dualNum stack + 2}) ["\t" ++ (show (dualNum stack)) ++ " CALL_FUNCTION " ++ (show nb_args)]

    callByteCode :: Ast -> Env -> Stack -> Maybe Stack
    callByteCode (Call (SymbolAst x : xs)) env stack = case (getToken x) of
        Just s -> case checkSymbols xs of
            True -> case preCreateCallByteCode xs env stack  of
                Just newStack -> case addFormulas newStack ((SymbolAst x : xs)) of
                    Just finalStack -> Just finalStack { bytecode = (init (bytecode finalStack)) ++ ["\t" ++ (show (dualNum finalStack)) ++ "\t" ++ (last (bytecode finalStack)) ++ s, ""], dualNum = dualNum finalStack + 2 }
                Nothing -> error "Should not be happening"
            False -> case evalAst (Call (SymbolAst x : xs)) env of
                Right (ast, n_env) -> case loadConst stack ast of
                    Just newStack -> Just newStack
                    Nothing -> error "Should not be happening"
                Left err -> error "Should not be happening"
        Nothing -> case addFunction stack x of
            Just newStack -> case preCreateCallByteCode xs env newStack of
                Just postStack -> case addFormulas postStack xs of
                    Just finalStack -> Just finalStack
                    Nothing -> error "Not implemented yet"
                Nothing -> error "Not implemented yet"
            Nothing -> error "Not implemented yet"
    callByteCode _  _ stack = Just (addByteCode (stack { dualNum = dualNum stack + 4, end = True }) ["\t" ++ (show (dualNum stack)) ++ " LOAD_CONST 0 (None)", "\t" ++ (show (dualNum stack + 2)) ++ " RETURN_VALUE"])

    symbolByteCode :: Ast -> Env -> Stack -> Maybe Stack
    symbolByteCode (SymbolAst s) env stack = case checkList s (fast stack) of
        True -> case loadFast stack s of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening"
        False -> case (loadGlobal stack s) of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening"
    symbolByteCode _ _ _ = error "Should not be happening"

    constByteCode :: Ast -> Env -> Stack -> Maybe Stack
    constByteCode (FloatAst f) env stack = case loadConst stack (FloatAst f) of
        Just s2 -> Just s2
        _ -> error "Should not be happening"
    constByteCode (IntegerAst i) env stack = case loadConst stack (IntegerAst i) of
        Just s2 -> Just s2
        _ -> error "Should not be happening"

    
    defineByteCode :: Ast -> Env -> Stack -> Maybe Stack
    defineByteCode (Define name ast) env stack = case ast of
        SymbolAst s -> case symbolByteCode ast env stack of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening" 
        FloatAst f -> case constByteCode ast env stack of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening"
        IntegerAst i -> case constByteCode ast env stack of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening"
        Call (SymbolAst x : xs) -> case callByteCode ast env stack of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening"
        Lambda ( x : xs ) ast -> error (show ast)
        _ -> error "Should not be happening"
    defineByteCode _ _ _ = error "Should not be happening"
    
    evalTrueCond :: Ast -> Env -> Stack -> Maybe Stack
    evalTrueCond (Define name ast) env stack = case defineByteCode (Define name ast) env stack of
        Just newStack -> Just newStack
        Nothing -> error "Should not be happening"
    evalTrueCond (Call (SymbolAst x : xs )) env stack = case getToken x of
        Just s -> Just stack
        Nothing -> case callByteCode (Call ( SymbolAst x : xs )) env stack of
            Just newStack -> Just newStack
            Nothing -> error "Should not be happening"
    evalTrueCond (SymbolAst s) env stack = case symbolByteCode (SymbolAst s) env stack of
        Just newStack -> Just newStack
        Nothing -> error "Should not be happening"
    evalTrueCond _ _ stack = Just stack

    calculateJump :: Int -> [String] -> [String] -> Int
    calculateJump nb1 [] [] = nb1
    calculateJump nb1 bytecode1 bytecode2 = nb1 + ((length bytecode2 - length bytecode1) * 2) + 4

    ifByteCode :: Ast -> Env -> Stack -> Stack
    ifByteCode (If cond true false) env stack = case cond of
        SymbolAst s -> case symbolByteCode cond env stack of
            Just newStack -> case evalTrueCond true env newStack of
                Just newStack2 -> case evalTrueCond true env (addByteCode (newStack {dualNum = dualNum newStack + 2}) ["\t" ++ (show (dualNum newStack)) ++ "\tPOP_JUMP_IF_FALSE " ++ (show (calculateJump (dualNum newStack) (bytecode newStack) (bytecode newStack2)))]) of
                    Just newStack3 -> case evalTrueCond false env (addByteCode (newStack3 {dualNum = dualNum newStack3 + 2}) ["\t" ++ show (dualNum newStack) ++ "\tJUMP_FORWARD 0\t\t(to " ++ (show (dualNum newStack3 + 2)) ++ ")"]) of
                        Just newStack4 -> newStack4
                        Nothing -> error "Should not be happening"
                    Nothing -> error "Should not be happening"
            Nothing -> error "Should not be happening"
        IntegerAst i -> case evalTrueCond false env stack of
            Just newStack -> newStack
            Nothing -> error "Should not be happening"
        FloatAst f -> case evalTrueCond false env stack of
            Just newStack -> newStack
            Nothing -> error "Should not be happening"
        Call (SymbolAst x : xs ) -> case getToken x of
            Just token -> addByteCode (stack { dualNum = dualNum stack + 4, end = True } ) ["\t" ++ (show (dualNum stack)) ++ " LOAD_CONST 0\t\t(None)", "\t" ++ (show (dualNum stack + 2)) ++ " RETURN_VALUE"]
            Nothing -> case (callByteCode (Call (SymbolAst x : xs )) env stack) of
                Just newStack -> case evalTrueCond true env newStack of
                    Just newStack2 -> case evalTrueCond true env (addByteCode (newStack {dualNum = dualNum newStack + 4}) ["\t" ++ (show (dualNum newStack)) ++ "\tCALL_FUNCTION " ++ (show (length xs)),"\t" ++ (show (dualNum newStack + 2)) ++ "\tPOP_JUMP_IF_FALSE " ++ (show (calculateJump (dualNum newStack) (bytecode newStack) (bytecode newStack2) + 2))]) of
                        Just newStack3 -> case evalTrueCond false env (addByteCode (newStack3 {dualNum = dualNum newStack3 + 2}) ["\t" ++ show (dualNum newStack3) ++ "\tJUMP_FORWARD 0\t\t(to " ++ (show (dualNum newStack3 + 2)) ++ ")"]) of
                            Just newStack4 -> newStack4
                            Nothing -> error "Should not be happening"
                        Nothing -> error "Should not be happening"
                Nothing -> error "Should not be happening"
    ifByteCode _ _ _ = error "Should not be happening"
    
    whileByteCode :: Ast -> Env -> Stack -> Stack
    whileByteCode (While ast insider) env stack = case ast of
        SymbolAst sym -> case evalTrueCond (SymbolAst sym) env stack of
            Just newStack -> newStack
            Nothing -> error "Should not be happening"
        Call (SymbolAst x : xs ) -> case callByteCode (Call (SymbolAst x : xs )) env stack of
            Just newStack -> case getToken x of
                Just s -> addByteCode (newStack {dualNum = dualNum newStack + 4}) ["\t" ++ (show (dualNum newStack)) ++ "\t" ++ s, "\t" ++ (show (dualNum newStack + 2)) ++ "\tPOP_JUMP_IF_FALSE " ++ (show (dualNum newStack + 4))]
                Nothing -> case evalTrueCond insider env newStack of
                    Just newStack2 -> addByteCode (newStack2 {dualNum = dualNum newStack2 + 2}) ["\t" ++ (show (dualNum newStack2)) ++ "\tJUMP_ABSOLUTE " ++ (show (dualNum newStack2 + 2))]
                    Nothing -> error "Should not be happening"
            Nothing -> error "Should not be happening"
        _ -> case (evalTrueCond insider env stack) of
            Nothing -> error "Should not be happening"
            Just newStack -> addByteCode (newStack {dualNum = dualNum newStack + 2}) ["\t" ++ (show (dualNum newStack)) ++ "\tJUMP_ABSOLUTE\t" ++ (show (dualNum newStack))]
    whileByteCode _ _ _ = error "Should not be happening"

    addFastArgs :: [String] -> [Ast] -> Stack -> Stack
    addFastArgs _ [] stack = stack
    addFastArgs [] _ stack = stack
    addFastArgs ( x : xs ) ( y : ys ) stack = case loadConst stack y of
        Just newStack -> case loadFast newStack x of
            Just finalStack -> addFastArgs xs ys finalStack
            Nothing -> error "Should not happening"
        Nothing -> error "Should not happening"

    createByteCode :: Ast -> Env -> Stack -> Stack
    createByteCode (SymbolAst s) env stack = case symbolByteCode (SymbolAst s) env stack of
        Just newStack -> newStack
        Nothing -> error "Should not be happening"
    createByteCode (Define name ast) env stack = case defineByteCode (Define name ast) env stack of
        Just newStack -> case loadFast newStack name of
            Just finalStack -> finalStack
            Nothing -> error "Should not be happening"
        Nothing -> error "Should not be happening"
    createByteCode (Call (SymbolAst x : xs)) env stack = case getToken x of
        Just token -> case callByteCode (Call (SymbolAst x : xs)) env stack of
            Just newStack -> addByteCode (newStack {dualNum = dualNum newStack + 2}) ["\t" ++ (show (dualNum newStack)) ++ "\tPOP_TOP"]
            Nothing -> error "Should not be happening" 
        Nothing -> case getValueEnv env x of
            Left err -> case callByteCode (Call (SymbolAst x : xs)) env stack of
                    Just newStack -> addByteCode (newStack {dualNum = dualNum newStack + 4}) ["\t" ++ (show (dualNum newStack)) ++ "\tCALL_FUNCTION " ++ (show (length xs)), "\t" ++ (show (dualNum newStack + 2)) ++ "\tPOP_TOP"]
                    Nothing -> error "Should not happening"
            Right ast -> case ast of
                ArgsLambda (list, ast) -> case addFastArgs list xs stack of
                    fastStack -> addByteCode (fastStack {dualNum = dualNum fastStack + 4}) ["\t" ++ (show (dualNum fastStack)) ++ "\tCALL_FUNCTION " ++ (show (length xs)), "\t" ++ (show (dualNum fastStack + 2)) ++ "\tPOP_TOP"]
                _ -> case callByteCode (Call (SymbolAst x : xs)) env stack of
                    Just newStack -> addByteCode (newStack {dualNum = dualNum newStack + 4}) ["\t" ++ (show (dualNum newStack)) ++ "\tCALL_FUNCTION " ++ (show (length xs)), "\t" ++ (show (dualNum newStack + 2)) ++ "\tPOP_TOP"]
                    Nothing -> error "Should not happening"
    createByteCode (If cond true false) env stack = ifByteCode (If cond true false) env stack
    createByteCode (Lambda (x : xs) ast) env stack = addByteCode (stack {dualNum = dualNum stack + 2, global = global stack ++ [x]}) ["\t" ++ (show (dualNum stack)) ++ "\tSTORE_GLOBAL " ++ (show (length (global stack))) ++ "\t\t(" ++ x ++ ")"]
    createByteCode (While cond ast) env stack = whileByteCode (While cond ast) env stack
    createByteCode ast env stack = error (show ast)
    -- createByteCode _ _ stack = addByteCode (stack { dualNum = dualNum stack + 4, end = True } ) ["\t" ++ (show (dualNum stack)) ++ " LOAD_CONST 0\t\t(None)", "\t" ++ (show (dualNum stack + 2)) ++ " RETURN_VALUE"]
