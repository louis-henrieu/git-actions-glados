data Cpt = Number Int
        | Symbol String
        | List [Cpt]
        deriving Show

data Ast = Define String Ast
    | DefineAlt [String] Ast
    | IntegerAst Int
    -- | FloatAst Float
    | SymbolAst String
    | Lambda [String] Ast
    -- | ArgsLambda ([String], Ast)
    | If Ast Ast Ast
    -- | Builtin ([Ast] -> Env -> Either String Ast)
    | Call [Ast]
    -- | Empty
    deriving Show

checkOnlySymbols :: [Cpt] -> Either String Bool
checkOnlySymbols [] = Right True
checkOnlySymbols (x:xs) = case x of
    Symbol _ -> checkOnlySymbols xs
    _ -> Left ("Error :" ++ show x ++ " is not a symbol")

parsingDefine :: Cpt -> Either String Ast
parsingDefine (List (Symbol "define":Symbol s:xs)) = case length xs of
    1 -> case cptToAst (head xs) of
        Left err -> Left ("The ast part of the define is invalid: " ++ err)
        Right ast -> Right (Define s ast)
    _ -> Left "The define form is invalid"
parsingDefine (List (Symbol "define":List x:xs)) = 
    if (length xs == 1) && (checkOnlySymbols x == Right True) then
        (case cptToAst (head xs) of
            Left err -> Left ("The ast part of the define is invalid: " ++ err)
            Right ast -> Right (DefineAlt (map (\ (Symbol s) -> s) x) ast))
    else
        Left "The define form is invalid"
parsingDefine _ = Left "The define form is invalid"

parsingList :: Cpt -> Either String Ast
parsingList (List (Symbol "define":xs)) = parsingDefine (List (Symbol "define":xs))
parsingList (List (Symbol "lambda":List x:xs)) = if length xs == 1 then 
    if null x then
        case cptToAst (head xs) of
            Left err -> Left ("The ast part of the lambda is invalid: " ++ err)
            Right ast -> case ast of
                SymbolAst s -> Right (Lambda [] (SymbolAst s))
                Call s -> Right (Lambda [] (Call s))
                _ -> Left "The lambda form is invalid"
        else if checkOnlySymbols x == Right True then
            case cptToAst (head xs) of
                Left err -> Left ("The ast part of the lambda is invalid: " ++ err)
                Right ast -> case ast of
                    SymbolAst s -> Right (Lambda (map (\(Symbol s) -> s) x) (SymbolAst s))
                    Call s -> Right (Lambda (map (\(Symbol s) -> s) x) (Call s))
                    _ -> Left "The lambda form is invalid"
            else
                Left "The lambda form is invalid"
    else
        Left "The lambda form is invalid"
parsingList (List (Symbol "if":x:y:z:xs)) = if null xs then
    case cptToAst x of
        Left err -> Left ("The ast part of the if is invalid: " ++ err)
        Right ast -> case cptToAst y of
            Left err -> Left ("The ast part of the if is invalid: " ++ err)
            Right ast2 -> case cptToAst z of
                Left err -> Left ("The ast part of the if is invalid: " ++ err)
                Right ast3 -> Right (If ast ast2 ast3)
    else
        Left "The if form is invalid"
parsingList (List s) = Right (Call (map (\x -> case cptToAst x of
    Left err -> error err
    Right ast -> ast) s))
parsingList _ = Left "Error in cptToAst: this is not a valid list"

cptToAst :: Cpt -> Either String Ast
cptToAst (Number n) = Right (IntegerAst n)
cptToAst (Symbol s) = Right (SymbolAst s)
cptToAst (List x) = parsingList (List x)

main :: IO ()
main = do
    -- test (if (== x 1) 2 3)
    putStrLn (show (cptToAst (List [Symbol "if", List[Symbol "==", Symbol "x", Number 1], Number 2, Number 3])))
    
    -- test (define x 1)
    putStrLn (show (cptToAst (List [Symbol "define", Symbol "x", Number 1])))

    -- test (define (x y) 1)
    putStrLn (show (cptToAst (List [Symbol "define", List [Symbol "x", Symbol "y"], Number 1])))

    -- test (lambda (x y) 1)
    putStrLn (show (cptToAst (List [Symbol "lambda", List [Symbol "x", Symbol "y"], Number 1])))

    -- test (Number 1)
    putStrLn (show (cptToAst (Number 1)))

    -- test (Symbol "x")
    putStrLn (show (cptToAst (Symbol "x")))

    -- test (List [(Symbol "+", Number 1, Number 2)])
    putStrLn (show (cptToAst (List [Symbol "+", Number 1, Number 2])))

    -- test (List [(Symbol "+", List[(Symbol "x"), Number 1, Number 2)], Number 3)
    putStrLn (show (cptToAst (List [Symbol "+", List[Symbol "x", Number 1, Number 2], Number 3])))
    