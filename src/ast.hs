module AST where
    import CPT

    data Ast = Define String (Maybe Ast)
        | IntegerAst Int
        | SymbolAst String
        | Call [Ast]
        deriving Show

    -- listCptToListAST' :: Cpt -> Ast
    -- listCptToListAST' l = case l of
    --     (List x) -> (Call (listCptToListAST' x))
    --     (List (Symbol "define"):(Symbol x):(Number y)) ->(Define x (Value y))
    --     (Number x)->(Value x)
    --     (Symbol x)->(Symb x)


    -- listCptToListAST :: [Cpt] -> [Ast]
    -- listCptToListAST l = case l of
    --     [] -> []
    --     (List x):l -> (Call (listCptToListAST x)) : listCptToListAST l
    --     (Symbol "define"):(Symbol x):(Number y):l
    --         ->(Define x (Value y)):listCptToListAST l
    --     (Number x):l
    --         ->(Value x):listCptToListAST l
    --     (Symbol x):l
    --         ->(Symb x):listCptToListAST l

    --fixAstList :: [Ast] -> [Maybe Ast]
    --fixAstList l = case l of
    --    [] -> []
    --    (Define x, y):(c):l -> Nothing
    --    (Define x, y):l -> fixAstList l
    --    (Call x) : l -> (fixAstList x) : fixAstList l


    --callListAst :: [Maybe Ast] -> Maybe Ast
    --callListAst l = case l of
    --    [] -> Nothing
    --    x -> Just (Call x)

    -- cptToAST :: Cpt -> Ast
    -- cptToAST (List (Symbol "define"):(Symbol x):y) = Define x (cptToAST y)
    -- cptToAst (List (Symbol "define", 
    -- cptToAST (Number x) = Just (Value x)
    -- cptToAST (Symbol x) = Just (Symb x)
    -- cptToAST (List x) = Just (Call (listCptToListAST x))

    affirmJust :: Maybe a -> a
    affirmJust (Just a) = a

    cptToAST :: Cpt -> Maybe Ast
    cptToAST (Number i) = Just (IntegerAst i)
    cptToAST (Symbol s) = Just (SymbolAst s)
    cptToAST (List (Symbol "define" : Symbol s : cpt : [])) = Just (Define s (cptToAST cpt))
    cptToAST (List (Symbol "define" : _)) = Nothing
    cptToAST (List l) = Just (Call (map affirmJust (map cptToAST l)))

    addFunction :: [Ast] -> Ast
    addFunction [] = error "Empty list"
    -- addFunction (Call x : xs) = addFunction ((evalAST (Call x)) : xs)
    addFunction (SymbolAst x : xs) = error "Not a number"
    addFunction (Define x y : xs) = error "Not a number"
    addFunction list = IntegerAst (sum [x | IntegerAst x <- list])

    minusFunction :: [Ast] -> Ast
    minusFunction [] = error "Empty list"
    -- minusFunction (Call x : xs) = minusFunction ((evalAST (Call x)) : xs)
    minusFunction (SymbolAst x : xs) = error "Not a number"
    minusFunction (Define x y : xs) = error "Not a number"
    minusFunction list = IntegerAst (foldl1 (-) [x | IntegerAst x <- list])

    mulFunction :: [Ast] -> Ast
    mulFunction [] = error "Empty list"
    -- mulFunction (Call x : xs) = mulFunction ((evalAST (Call x)) : xs)
    mulFunction (SymbolAst x : xs) = error "Not a number"
    mulFunction (Define x y : xs) = error "Not a number"
    mulFunction list = IntegerAst (product [x | IntegerAst x <- list])

    divFunction :: [Ast] -> Ast
    divFunction [] = error "Empty list"
    -- divFunction (Call x : xs) = divFunction ((evalAST (Call x)) : xs)
    divFunction (SymbolAst x : xs) = error "Not a number"
    divFunction (Define x y : xs) = error "Not a number"
    divFunction list = IntegerAst (foldl1 div [x | IntegerAst x <- list])

    modFunction :: [Ast] -> Ast
    modFunction [] = error "Empty list"
    -- modFunction (Call x : xs) = modFunction ((evalAST (Call x)) : xs)
    modFunction (SymbolAst x : xs) = error "Not a number"
    modFunction (Define x y : xs) = error "Not a number"
    modFunction list = IntegerAst (foldl1 mod [x | IntegerAst x <- list])

    getValueEnv :: [(String, (Maybe Ast))] -> String -> Either String (Maybe Ast)
    getValueEnv [] key = Left (key ++ " was not defined in the environment")
    getValueEnv ((_key, _value):xs) key = if key == _key
        then Right _value
        else getValueEnv xs key

    defineValue :: Ast -> [(String, (Maybe Ast))] -> [(String, (Maybe Ast))]
    defineValue (Define x y) l = (x, y) : l
    defineValue _ l = l

    evalAST :: Ast -> Either String Ast
    evalAST (IntegerAst x) = Right (IntegerAst x)
    evalAST (SymbolAst x) = Left("Need : " ++ x ++ " is a symbol to find")
    evalAST (Call (SymbolAst x : xs)) = case x of
        "+" -> Right (addFunction xs)
        -- "-" -> (minusFunction xs)
        -- "*" -> (mulFunction xs)
        "/" -> Right (divFunction xs)
        "%" -> Right (modFunction xs)
        _ -> Left ("Error : " ++ x ++ " is not a function")
    evalAST _ = Left("Error : Not a function")


    -- evalAst (Call (Call x : xs)) = evalAst (Call (evalAst (Call x) : xs))
    -- evalAst (Define x y) = Left("Need : Define")
    -- evalAst (SymbolAst x) = Left("Need :" ++ x ++ "Symbol to find")
    -- evalAst (Call []) = Left("Error : empty call")
    -- evalAst _ = Left("Error : Not a function")