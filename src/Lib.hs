module Lib
    ( 
        next,
        --nextApp,
        --aaInt,
        getSh,
        someFunc
    ) where

import Temp
import CPT
import AST
import ParseCpt

next :: Int -> Int
next i = i + 1

--aaInt :: String -> Char
--aaInt (x:_) = x
--aaInt _ = ' '
--
getSh :: Char -> IO ()
getSh x = print (nexti x)


someFunc :: Env -> IO ()
someFunc = do
    line <- getLine
    if line == "quit" then return() else do
            let cpts = parseCpt line
            let cpt_ast = cptToAST (head cpts)
            case cpt_ast of
                Just ast -> case ast of
                    (IntegerAst i) -> print i
                    _ -> case evalAST ast of
                        Right result -> case result of
                            (IntegerAst i) -> print i
                            _ -> print result
                        Left err -> print err
                Nothing -> print "The AST isn't valid"
            someFunc
                -- -- Just ast -> print ast
                -- Just ast -> case evalAST ast of
                --     Right result -> print result
                --     Left err -> print err
                -- Nothing -> print "Error"

-- getEnv :: k -> [(v, k)] -> Maybe v
-- getEnv _ [] = Nothing
-- getEnv k [(value, key):(vs, ks)] =
--     | k == key = Just(value)
--     | otherwise = getEnv k (vs, ks)
