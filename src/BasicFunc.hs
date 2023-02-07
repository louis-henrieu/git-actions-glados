module BasicFunc (
    checkFloatInt,
    preAdd,
    preSub,
    preMul,
    checkZero,
    preDiv,
    preMod,
    fact
) where

import Info

checkFloatInt :: [Ast] -> Env -> Either String Bool
checkFloatInt [] env = Right True
checkFloatInt (x:xs) env = case x of
    (IntegerAst i) -> checkFloatInt xs env
    (FloatAst f) -> checkFloatInt xs env
    _ -> Left "The arg \'x\' is not a number"


preAdd :: [Ast] -> Env -> Either String Ast
preAdd [] env = Left "Add function needs at least two arguments"
preAdd args env = case checkIfEmpty (convertArgs args env) of
    True -> case checkFloatInt (convertArgs args env) env of
        Right True -> add (convertArgs args env) env
        Left err -> Left err
    False -> Left "There is at least a symbol that doesn't exist"

add :: [Ast] -> Env -> Either String Ast
add [] env = Right (IntegerAst 0)
add (x:xs) env = case x of
    (IntegerAst i) -> Right (IntegerAst (i + (sum [x | IntegerAst x <- xs])))
    (FloatAst f) -> Right (FloatAst (f + (sum [x | FloatAst x <- xs])))
    _ -> Left "Not a number"

preSub :: [Ast] -> Env -> Either String Ast
preSub [] env = Left "Sub function needs at least two arguments"
preSub args env = case checkIfEmpty (convertArgs args env) of
    True -> case checkFloatInt (convertArgs args env) env of
        Right True -> sub (convertArgs args env) env
        Left err -> Left err
    False -> Left "There is at least a symbol that doesn't exist"

sub :: [Ast] -> Env -> Either String Ast
sub [] env = Right (IntegerAst 0)
sub (x:xs) env = case x of
    (IntegerAst i) -> Right (IntegerAst (i - (sum [x | IntegerAst x <- xs])))
    (FloatAst f) -> Right (FloatAst (f - (sum [x | FloatAst x <- xs])))
    _ -> Left "Not a number"

preMul :: [Ast] -> Env -> Either String Ast
preMul [] env = Left "Mul function needs at least two arguments"
preMul args env = case checkIfEmpty (convertArgs args env) of
    True -> case checkFloatInt (convertArgs args env) env of
        Right True -> mul (convertArgs args env) env
        Left err -> Left err
    False -> Left "There is at least a symbol that doesn't exist"

mul :: [Ast] -> Env -> Either String Ast
mul [] env = Right (IntegerAst 1)
mul (x:xs) env = case x of
    (IntegerAst i) -> Right (IntegerAst (i * (product [x | IntegerAst x <- xs])))
    (FloatAst f) -> Right (FloatAst (f * (product [x | FloatAst x <- xs])))
    _ -> Left "Not a number"

checkZero :: Ast -> Env -> Either String Ast
checkZero (IntegerAst i) env = case i of
    0 -> Left "Second argument is null"
    _ -> Right (IntegerAst i)
checkZero (FloatAst f) env = case f of
    0 -> Left "Second argument is null"
    _ -> Right (FloatAst f)
checkZero (SymbolAst s) env = case getValueEnv env s of
    Right ast -> case ast of
        (IntegerAst i) -> case i of
            0 -> Left "Second argument is null"
            _ -> Right (IntegerAst i)
        (FloatAst f) -> case f of
            0 -> Left "Second argument is null"
            _ -> Right (FloatAst f)
        _ -> Left "The symbol \'s\' isn't valid"
    Left err -> Left err
checkZero _ env = Left "Not a number"

preDiv :: [Ast] -> Env -> Either String Ast
preDiv [] env = Left "Div function needs at least two arguments"
-- no more than two arguments
preDiv (x:y:xs) env = case length (x:y:xs) of
    2 -> case checkIfEmpty (convertArgs (x:y:xs) env) of
        True -> case checkFloatInt (convertArgs (x:y:xs) env) env of
            Right True -> case checkZero (y) env of
                Right ast -> (division (convertArgs [x, y] env) (env))
                Left err -> Left err
            Left err -> Left err
        False -> Left "There is at least a symbol that doesn't exist"
    _ -> Left "Div function only needs two arguments"
division :: [Ast] -> Env -> Either String Ast
division [] env = Right (IntegerAst 1)
division list env = Right (IntegerAst (foldl1 div [x | IntegerAst x <- list]))

checkOnlyInt :: [Ast] -> Env -> Bool
checkOnlyInt [] env = True
checkOnlyInt (x:xs) env = case x of
    (IntegerAst i) -> checkOnlyInt xs env
    _ -> False

preMod :: [Ast] -> Env -> Either String Ast
preMod [] env = Left "Mod function needs at least two arguments"
preMod (x:y:xs) env = case length (x:y:xs) of
    2 -> case checkIfEmpty (convertArgs (x:y:xs) env) of
        True -> case checkOnlyInt (convertArgs (x:y:xs) env) env of
            True -> case checkZero (y) env of
                Right ast -> modulo (convertArgs (x:y:xs) env) env
                Left err -> Left err
            False -> Left "Mod function only works with integers"
        False -> Left "There is at least a symbol that doesn't exist"
    _ -> Left "Mod function only needs two arguments"

modulo :: [Ast] -> Env -> Either String Ast
modulo (IntegerAst x: IntegerAst y: xs) env = Right (IntegerAst (x `mod` y))
modulo _ _ = Left "Not a number"

fact :: Int -> Ast
fact x = IntegerAst (product [1..x])