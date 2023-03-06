module Cpt 
        (
            Cpt(..),
            printTree
        ) where


data Cpt = Number Int
        | NumberFloat Float
        | Symbol String
        | List [Cpt]
        | Separator String
        deriving Show

printTree :: Cpt -> String
printTree x = case x of
    (Symbol s) -> "a Symbol '" ++ s ++ "'"
    (Number i) -> "a Number '" ++ show i ++ "'"
    (NumberFloat f) -> "a Number '" ++ show f ++ "'"
    (List l) -> "a List with " ++ unwords (map printTree l) ++ ""
    (Separator _) -> "a Separator"
