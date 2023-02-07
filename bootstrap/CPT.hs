module CPT where


data Cpt = Number Int
        | Symbol String
        | List [Cpt]
        deriving Show

printTree :: Cpt -> String
printTree x = case x of
    (Symbol x) -> "a Symbol '" ++ x ++ "'"
    (Number x) -> "a Number '" ++ show x ++ "'"
    (List x) -> "a List with " ++ unwords (map printTree x) ++ ""


--data Ast = Define String (Maybe Ast)
--        | Value Int
--        | Symb String
--        | Call [Ast]
--        deriving Show
--
--cptToAst :: Cpt -> Maybe Ast
--cptToAst (Number x) = Just (Value x)
--cptToAst (Symbol x) = Just (Symb x)
--cptToAst (List (Symbol "define" : Symbol x : y : rest))
--    | null rest = Just (Define x (cptToAst y))
--    | otherwise = Nothing
--cptToAst (List (Symbol "define" : _)) = Nothing
--cptToAst (List x) = case tab of
--        Just t -> Just (Call t)
--        where tab = (sequence (map cptToAst x))



--addFunction :: [Ast] -> Ast
--addFunction [] = error "Empty list"
--addFunction (Call x : xs) = addFunction ((evalAST (Call x)) : xs)
--addFunction (Symb x : xs) = error "Not a number"
--addFunction (Define x y : xs) = error "Not a number"
--addFunction list = Value (sum [x | Value x <- list])
--
--minusFunction :: [Ast] -> Ast
--minusFunction [] = error "Empty list"
--minusFunction (Call x : xs) = minusFunction ((evalAST (Call x)) : xs)
--minusFunction (Symb x : xs) = error "Not a number"
--minusFunction (Define x y : xs) = error "Not a number"
--minusFunction list = Value (foldl1 (-) [x | Value x <- list])
--
--mulFunction :: [Ast] -> Ast
--mulFunction [] = error "Empty list"
--mulFunction (Call x : xs) = mulFunction ((evalAST (Call x)) : xs)
--mulFunction (Symb x : xs) = error "Not a number"
--mulFunction (Define x y : xs) = error "Not a number"
--mulFunction list = Value (product [x | Value x <- list])
--
--divFunction :: [Ast] -> Ast
--divFunction [] = error "Empty list"
--divFunction (Call x : xs) = divFunction ((evalAST (Call x)) : xs)
--divFunction (Symb x : xs) = error "Not a number"
--divFunction (Define x y : xs) = error "Not a number"
--divFunction list = Value (foldl1 div [x | Value x <- list])
--
--evalAST :: Ast -> Ast
--evalAST (Value x) = (Value x)
--evalAST (Call (Symb x : xs)) = case x of
--    "+" -> (addFunction xs)
--    "-" -> (minusFunction xs)
--    "*" -> (mulFunction xs)
--    "/" -> (divFunction xs)
--    otherwise -> error "Unknown function"
