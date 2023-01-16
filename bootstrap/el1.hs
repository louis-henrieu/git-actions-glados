module Part1 where

data Cpt = Number Int
        | Symbol String
        | List [Cpt]
        deriving Show

printTree :: Cpt -> String
printTree x = case x of
    (Symbol x) -> "a Symbol '" ++ x ++ "'"
    (Number x) -> "a Number '" ++ show x ++ "'"
    (List x) -> "a List with " ++ unwords (map printTree x) ++ ""


data Ast = Define String (Maybe Ast)
        | Value Int
        | Symb String
        | Call [Ast]
        deriving Show

cptToAst :: Cpt -> Maybe Ast
cptToAst (Number x) = Just (Value x)
cptToAst (Symbol x) = Just (Symb x)
cptToAst (List (Symbol "define" : Symbol x : y : rest))
    | null rest = Just (Define x (cptToAst y))
    | otherwise = Nothing
cptToAst (List (Symbol "define" : _)) = Nothing
cptToAst (List x) = case tab of
        Just t -> Just (Call t)
        where tab = (sequence (map cptToAst x))



-- evalAST


addFunction :: [Ast] -> Ast
addFunction [] = error "Empty list"
addFunction (Call x : xs) = addFunction ((evalAST (Call x)) : xs)
addFunction list = Value (sum [x | Value x <- list])


mulFunction :: [Ast] -> Ast
mulFunction [] = error "Empty list"
mulFunction (Call x : xs) = mulFunction ((evalAST (Call x)) : xs)
mulFunction list = Value (product [x | Value x <- list])


evalAST :: Ast -> Ast
evalAST (Value x) = (Value x)
-- evalAST (Call (Symb "+" : x)) = Just (addFunction (x))
-- evalAST (Call (Symb "*" : x)) = Just (mulFunction (x))
evalAST (Call (Symb x : xs)) = case x of
    "+" -> (addFunction xs)
    "*" -> (mulFunction xs)
    otherwise -> error "Unknown function"
