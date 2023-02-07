module Lambda (

) where
    -- lambda [Ast] Ast -> Either String Ast
    -- lambda should become a builtin function
    preLambda :: Ast -> Env -> Either String Ast