import Data.List

data Cpt = Num Int | Node [Cpt] | Symbole String deriving Show

-- data Cpt = Num Int | Word String | Node [String] | Symbole (String, [Cpt]) deriving Show
-- => [
-- ( define x 5) => ["define", (x, []), 5]
-- x => (x, [5])
-- ( if ( > x 4) 1 0) => ["if", [">" (x,[5]), 4], 1, 0]
-- ( define y (+ 5 x ) ) => ["define", (y, []) ["+", 5, (x, [5])] ]
-- => ]

-- [
--    ["define", (x, []), 5],
--    (x, [5]),
--    ["if", [">" (x,[5]), 4], 1, 0],
--    ["define", (y, []) ["+", 5, (x, [5])] ],
--]

myFst :: (a,b) -> a
myFst (a, b) = a

mySnd :: (a,b) -> b
mySnd (a,b) = b

getSymbol :: Cpt -> Maybe String
getSymbol (Symbole s) = Just s
getSymbol _ = Nothing

getInteger :: Cpt -> Maybe Int
getInteger (Num s) = Just s
getInteger _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (Node s) = Just s
getList _ = Nothing

affirmTrue :: Maybe a -> a
affirmTrue (Just x) = x

printTree :: Cpt -> String
printTree (Node s) = "a List with" ++ unwords (map printTree s)
printTree (Symbole s) = "a Symbol " ++ s
printTree (Num s) = "a Number " ++ show s

data Ast = Define String Cpt
        | Var Int
        | Call [Maybe Ast]
        | SymboleElse String
        deriving Show

cptToAST :: Cpt -> Maybe Ast
-- cptToAST (Node [Symbole s, Symbole x, Num y]) = case s of
--         "define" -> Just (Define x (Num y))
--         _ -> Just (SymboleElse s)
-- cptToAST (Node s) = Just (Call (map cptToAST s))

cptToAST (Num s) = Just (Var s)
cptToAST (Node []) = Nothing
cptToAST (Node s) = case s of
        [] -> Just (Call [])
        (Symbole "define"):(Symbole x):(Num y):s -> Just (Call ((Just (Define x (Num y))):(map cptToAST s ))) --Just (Call ((Just (Define x (Num y))):(map cptToAST s)))
        x:s -> (cptToAST (Node s)) --Just (Call ((cptToAST x) :(map cptToAST s)))
cptToAST (Symbole s) = Nothing
cptToAST (Symbole s) = case s of
        "define" -> 

-- Just ( Call ( ( Just ( Define x ( Num y ) ) ) : ( cptToAST ( Node s ) ) ) )