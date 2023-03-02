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
import Bytecode

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
        -- putStrLn (show (runParser (parseCpt) line))
        case runParser (parseCpt) line of
                Right (cpt, _) -> case cptToAst cpt of
                    Right ast -> case preEvalAst ast env of
                        Right ast_s -> case evalAst ast_s env of
                            Right result -> case (fst result) of
                                Empty -> someFuncGetLine (snd result)
                                _ -> printAst (fst result) >> someFuncGetLine (snd result)
                            Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                        Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                    Left err -> putStrLn ("Error : " ++ err) >> someFuncGetLine env
                Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)

printByteCode :: [String] -> IO()
printByteCode [] = return ()
printByteCode (x:xs) = do
    putStrLn x
    printByteCode xs

someFuncFile :: Env -> [String] -> Stack -> IO()
someFuncFile env [] stack = printByteCode (bytecode stack)
someFuncFile env (x:xs) stack = do
    case runParser (parseCpt) x of
        Right (cpt, _) -> case cptToAst cpt of
            Right ast -> case preEvalAst ast env of
                Right ast_s -> case evalAst ast_s env of
                    Right result -> case (fst result) of
                        Empty -> someFuncFile (snd result) xs (createByteCode ast (snd result) stack)
                        _ -> printAst (fst result) >> putStrLn "FUCK" >> someFuncFile (snd result) xs (createByteCode ast_s (snd result) stack)
                    Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
            Left err -> putStrLn ("Error : " ++ err) >> someFuncFile env xs stack
        Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)