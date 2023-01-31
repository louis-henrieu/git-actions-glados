module AST where
    import CPT

    data Ast = Define String (Ast)
        | Value Int
        | Symb String
        | Call [Ast]
        deriving Show

    -- listCptToListAST' :: Cpt -> Ast
    -- listCptToListAST' l = case l of
    --     (List x) -> (Call (listCptToListAST' x))
    --     (List (Symbol "define"):(Symbol x):(Number y)) ->(Define x (Value y))
    --     (Number x)->(Value x)
    --     (Symbol x)->(Symb x)


    -- listCptToListAST :: [Cpt] -> [Ast]
    -- listCptToListAST = map listCptToListAST'
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

    cptToAST :: Cpt -> Maybe Ast
    cptToAST (Number x) = Just (Value x)
    cptToAST (Symbol x) = Just (Symb x)

    addFunction :: [Ast] -> [(a, b)] -> Ast
    addFunction [] _ = error "Empty list"
    addFunction (Call x : xs) env = addFunction ((evalAST (Call x) env) : xs) env
    addFunction (Symb x : xs) _ = error "Not a number"
    addFunction (Define x y : xs) _ = error "Not a number"
    addFunction list _ = Value (sum [x | Value x <- list])

    minusFunction :: [Ast] -> [(a, b)] -> Ast
    minusFunction [] _ = error "Empty list"
    minusFunction (Call x : xs) env = minusFunction ((evalAST (Call x) env) : xs) env
    minusFunction (Symb x : xs) _ = error "Not a number"
    minusFunction (Define x y : xs) _ = error "Not a number"
    minusFunction list _ = Value (foldl1 (-) [x | Value x <- list])

    mulFunction :: [Ast] -> [(a, b)] -> Ast
    mulFunction [] _ = error "Empty list"
    mulFunction (Call x : xs) env = mulFunction ((evalAST (Call x) env) : xs) env
    mulFunction (Symb x : xs) _ = error "Not a number"
    mulFunction (Define x y : xs) _ = error "Not a number"
    mulFunction list _ = Value (product [x | Value x <- list])

    divFunction :: [Ast] -> [(a, b)] -> Ast
    divFunction [] _ = error "Empty list"
    divFunction (Call x : xs) env = divFunction ((evalAST (Call x) env) : xs) env
    divFunction (Symb x : xs) _ = error "Not a number"
    divFunction (Define x y : xs) _ = error "Not a number"
    divFunction list _ = Value (foldl1 div [x | Value x <- list])

    evalAST :: Ast -> [(a, b)] -> Ast
    evalAST (Value x) _ = (Value x)
    evalAST (Call (Symb x : xs)) env = case x of
        "+" -> (addFunction xs env)
        "-" -> (minusFunction xs env)
        "*" -> (mulFunction xs env)
        "/" -> (divFunction xs env)
        _ -> error "Unknown function"
    evalAst (Call (Call x : xs)) env = evalAST (Call (evalAST (Call x) env : xs)) env
    evalAst _ _ = error "Not a number"