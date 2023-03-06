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

--    checkPrrr :: [Cpt] -> Ast
--    checkPrrr [] = Empty
--    checkPrrr (x : xs) = Empty

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
 --   cptToAst (Separator x) = error "Holala ! There couldn't be a separator in cptToAst"
    cptToAst (NumberFloat f) = FloatAst f
    cptToAst (Number n) = IntegerAst n
    cptToAst (Symbol s) = SymbolAst s
    cptToAst (List x) = parseList (List x)
    cptToAst cpt = error (show (cpt))

    findSeparator :: [Cpt] -> Int -> Maybe Int
    findSeparator [] _ = Nothing
    findSeparator (x : xs) i = case x of
        Separator "=>" -> Just i
        _ -> findSeparator xs (i + 1)

    convertFunc :: [Cpt] -> Int -> [String] -> Either String [String]
    convertFunc _ 0 x = Right x
    --convertFunc [] _ x = Left "Holala ! There is a problem with the number of arguments"
  --  convertFunc [] _ _ = Left "Holala ! Can't convert an empty list"
    convertFunc (x : xs) i y = case x of
        Symbol s -> convertFunc xs (i - 1) (y ++ [s])
        _ -> Left ("Holala ! There is a non symbol in the list of arguments ! Argument is : " ++ show x ++ "and i is " ++ show i)

    convertList :: [Cpt] -> [Ast] -> Bool -> Either String [Ast]
    convertList [] [] _ = Left "Holala ! Can't convert an empty list"
    convertList [] x True = Right x
    --convertList [] x False = Left "Holala ! Missing a default case in the Prrr statement"
    convertList (x : xs) y bool = case x of
        List (ast : asts) -> case length asts of
                0 -> convertList xs (y ++ [cptToAst ast]) True
                _ -> case bool of
                    True -> Left "Holala ! You can't put states after the default case"
                    False -> case length asts of
                        1 -> convertList xs (y ++ [cptToAst (head asts)]) True
                        _ -> Left "Holala 3 ! There is a problem with the number of arguments in the Prrr statement + there is a default case"
        _ -> Left "Holala ! There can only be statement as list in the Prrr statement"

    specialCaseVerify :: [Cpt] -> Either String Ast
    specialCaseVerify (Symbol "Prrr"  : Symbol s : Symbol "of" : List x : xs) = case length xs of 
        0 -> case (convertList x [] False) of
            Left s -> Left s
            Right ast -> Right (Prrr s (Call ast))
        _ -> Left "Holala 1 ! There is a problem with the number of arguments in the Prrr statement"
    specialCaseVerify (Symbol "Siii" : x : y : z : xs ) = case length xs of
        0 -> Right (If (cptToAst x) (cptToAst y) (cptToAst z))
        _ -> Left "Holala 2 ! There is a problem with the number of arguments in the Siii statement"
    specialCaseVerify ( x : xs ) = Right (Call (map cptToAst (x : xs)))

    cptToAstList :: [Cpt] -> Either String Ast
    cptToAstList [] = Left "Holala ! Can't parse an empty list"
    cptToAstList x = case findSeparator x 0 of
        Just i -> case i of
            0 -> Left "Holala ! Seperator can't be the first element !!!"
            1 -> case length x of   
                3 -> case (head x) of
                    Symbol s -> Right (Define s (cptToAst (x !! 2)))
                    _ -> Left "Holala ! The first element of the list must be a symbol"
                _ -> Left ("Holala ! There is a length of " ++ show (length x))
            _ -> case convertFunc x i [] of
                Left s -> Left s
                Right f -> Right (Lambda f (cptToAst (x !! (i + 1))))
        Nothing -> case length x of
            1 -> Right (cptToAst (head x))
            _ -> case specialCaseVerify x of
                Right ast -> Right ast
                Left s -> Left s