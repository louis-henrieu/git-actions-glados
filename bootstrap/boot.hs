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

data Ast = Define String (Maybe Ast)
        | Lambda [Maybe Ast] String
        | Var Int
        | Call [Maybe Ast]
        | SymboleElse String
        deriving Show

cptToAst :: Cpt -> Maybe Ast
-- cptToAst (Node [Symbole s, Symbole x, Num y]) = case s of
--         "define" -> Just (Define x (Num y))
--         _ -> Just (SymboleElse s)
-- cptToAst (Node s) = Just (Call (map cptToAst s))

cptToAst (Num s) = Just (Var s)
cptToAst (Node []) = Nothing
cptToAst (Node s) = case s of
        [] -> Just (Call [])
        (Symbole "define"):(Symbole x):(Num y):s -> Just (Call ((Just (Define x (Num y))):(map cptToAst s ))) --Just (Call ((Just (Define x (Num y))):(map cptToAst s)))
        x:s -> (cptToAst (Node s)) --Just (Call ((cptToAst x) :(map cptToAst s)))
cptToAst (Symbole s) = Nothing
cptToAst (Symbole s) = case s of
        "define" -> 

-- Just ( Call ( ( Just ( Define x ( Num y ) ) ) : ( cptToAst ( Node s ) ) ) )
