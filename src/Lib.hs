module Lib
    (
        someFuncGetLine,
        someFuncFile
    ) where

import Ast
import Info
import Parser
import System.Exit
import System.IO (isEOF)

printAst :: Ast -> IO ()
printAst ast = case ast of
    (IntegerAst i) -> putStrLn (show i)
    (SymbolAst s) -> putStrLn s
    (FloatAst f) -> if f == (fromIntegral (round f :: Integer))
        then
            putStrLn (show (round f :: Integer))
        else
            putStrLn (show f)
    (Lambda _ _) -> print "#<procedure>"
    _ -> print "The Ast isn't valid"

someFuncGetLine :: Env -> IO()
someFuncGetLine env = do
    end <- isEOF
    if end
        then return ()
    else do
        line <- getLine
        putStrLn (show (runParser (parseCpt) line))
        -- case runParser (parseCpt) line of
        --         Right (cpt, _) -> case cptToAst cpt of
        --             Right ast -> case preEvalAst ast env of
        --                 Right ast_s -> case evalAst ast_s env of
        --                     Right result -> case (fst result) of
        --                         Empty -> someFuncGetLine (snd result)
        --                         _ -> printAst (fst result) >> someFuncGetLine (snd result)
        --                     Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
        --                 Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
        --             Left err -> putStrLn ("Error : " ++ err) >> someFuncGetLine env
        --         Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)

someFuncFile :: Env -> [String] -> IO()
someFuncFile _ [] = return ()
someFuncFile env (x:xs) = do
    case runParser (parseCpt) x of
        Right (cpt, _) -> case cptToAst cpt of
            Right ast -> case preEvalAst ast env of
                Right ast_s -> case evalAst ast_s env of
                    Right result -> case (fst result) of
                        Empty -> someFuncFile (snd result) xs
                        _ -> printAst (fst result) >> someFuncFile (snd result) xs
                    Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
            Left err -> putStrLn ("Error : " ++ err) >> someFuncFile env xs
        Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)