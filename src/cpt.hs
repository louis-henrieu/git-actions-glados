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
