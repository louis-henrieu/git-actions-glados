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
    