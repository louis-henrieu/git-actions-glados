module AST where
import CPT

data Ast = Define String (Ast)
        | Value Int
        | Symb String
        | Call [Ast]
        deriving Show

listCptToListAST :: [Cpt] -> [Ast]
listCptToListAST l = case l of
    [] -> []
    (List x):l -> (Call (listCptToListAST x)) : listCptToListAST l
    (Symbol "define"):(Symbol x):(Number y):l
        ->(Define x (Value y)):listCptToListAST l
    (Number x):l
        ->(Value x):listCptToListAST l
    (Symbol x):l
        ->(Symb x):listCptToListAST l

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
cptToAST (List x) = Just (Call (listCptToListAST x))

addFunction :: [Ast] -> Ast
addFunction [] = error "Empty list"
addFunction (Call x : xs) = addFunction ((evalAST (Call x)) : xs)
addFunction (Symb x : xs) = error "Not a number"
addFunction (Define x y : xs) = error "Not a number"
addFunction list = Value (sum [x | Value x <- list])

minusFunction :: [Ast] -> Ast
minusFunction [] = error "Empty list"
minusFunction (Call x : xs) = minusFunction ((evalAST (Call x)) : xs)
minusFunction (Symb x : xs) = error "Not a number"
minusFunction (Define x y : xs) = error "Not a number"
minusFunction list = Value (foldl1 (-) [x | Value x <- list])

mulFunction :: [Ast] -> Ast
mulFunction [] = error "Empty list"
mulFunction (Call x : xs) = mulFunction ((evalAST (Call x)) : xs)
mulFunction (Symb x : xs) = error "Not a number"
mulFunction (Define x y : xs) = error "Not a number"
mulFunction list = Value (product [x | Value x <- list])

divFunction :: [Ast] -> Ast
divFunction [] = error "Empty list"
divFunction (Call x : xs) = divFunction ((evalAST (Call x)) : xs)
divFunction (Symb x : xs) = error "Not a number"
divFunction (Define x y : xs) = error "Not a number"
divFunction list = Value (foldl1 div [x | Value x <- list])

evalAST :: Ast -> Ast
evalAST (Value x) = (Value x)
evalAST (Call (Symb x : xs)) = case x of
    "+" -> (addFunction xs)
    "-" -> (minusFunction xs)
    "*" -> (mulFunction xs)
    "/" -> (divFunction xs)
    _ -> error "Unknown function"
evalAst (Call (Call x : xs)) = evalAST (Call (evalAST (Call x) : xs))
evalAst _ = error "Not a number"