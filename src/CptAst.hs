module CptAst (
    cptToAst,
    cptToAstList
) where

    import Info
    import Cpt

    transFromListString :: [Cpt] -> [String]
    transFromListString [] = []
    transFromListString (x : xs) = case x of
        Symbol s -> s : transFromListString xs
        _ -> error "Holala ! Error in transFromListString"

    checkPrrr :: [Cpt] -> Ast
    checkPrrr [] = Empty
    checkPrrr (x : xs) = Empty

    parseList :: Cpt -> Ast
    parseList (List (Symbol x : Symbol ">" : y : xs)) = case length (y : xs) of
        1 -> Define x (cptToAst y)
        _ -> error "Holala ! Only one argument is allowed in define"
    parseList (List (List x : Symbol ">" : y : xs)) = case length (y : xs) of
        1 -> Lambda (transFromListString x) (cptToAst y)
        _ -> error "Holala ! Only one argument is allowed in define"
    parseList (List (Symbol "Siii" : x : y : xs)) = case length (xs) of
        1 -> If (cptToAst x) (cptToAst y) (cptToAst (head xs))
        _ -> error "Holala ! Only three arguments is allowed for a Siii statement"
    -- parseList (List (Symbol "Prrr" : List x)) = Empty
    parseList (List (Symbol "UnDeux" : Symbol "Yupi" : x : Symbol "Oulah" : xs)) = case length xs of
        1 -> Either (cptToAst x) (cptToAst (head xs))
        _ -> error "Holala ! Only two arguments is allowed for a UnDeux statement"
    parseList (List x) = Call (map cptToAst x)

    cptToAst :: Cpt -> Ast
    cptToAst (Separator x) = error "Holala ! There couldn't be a separator in cptToAst"
    cptToAst (NumberFloat f) = FloatAst f
    cptToAst (Number n) = IntegerAst n
    cptToAst (Symbol s) = SymbolAst s
    cptToAst (List x) = parseList (List x)
    cptToAst _ = error "Holala ! Error in cptToAst"

    findSeperator :: [Cpt] -> Int -> Maybe Int
    findSeperator [] _ = Nothing
    findSeperator (x : xs) i = case x of
        Separator "=>" -> Just i
        _ -> findSeperator xs (i + 1)

    convertFunc :: [Cpt] -> Int -> [String] -> Either String [String]
    convertFunc _ 0 x = Right x
    convertFunc [] _ x = Left "Holala ! There is a problem with the number of arguments"
    convertFunc [] _ _ = Left "Holala ! Can't convert an empty list"
    convertFunc (x : xs) i y = case x of
        Symbol s -> convertFunc xs (i - 1) (y ++ [s])
        _ -> Left ("Holala ! There is a non symbol in the list of arguments ! Argument is : " ++ show x ++ "and i is " ++ show i)

    cptToAstList :: [Cpt] -> Either String Ast
    cptToAstList [] = Left "Holala ! Can't parse an empty list"
    cptToAstList x = case findSeperator x 0 of
        Just i -> case i of
            0 -> Left "Holala ! Seperator can't be the first element !!!"
            1 -> case length x of   
                3 -> case ( head x) of
                    Symbol s -> Right (Define s (cptToAst (x !! 2)))
                    _ -> Left "Holala ! The first element of the list must be a symbol"
                _ -> Left ("Holala ! There si a length of " ++ show (length x))
            _ -> case convertFunc x i [] of
                Left s -> Left s
                Right f -> Right (Lambda f (cptToAst (x !! (i + 1))))
        Nothing -> case length x of
            1 -> Right (cptToAst (head x))
            _ -> Right (cptToAst (List x))
