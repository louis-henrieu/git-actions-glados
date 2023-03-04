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
    endF <- isEOF
    if endF
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
someFuncFile _ [] stack = case (end stack) of
    True ->  printByteCode (init (bytecode stack))
    False -> printByteCode ((bytecode stack) ++ ["LOAD_CONST 0", "RETURN_VALUE"]) >> exitWith ExitSuccess
someFuncFile env (x:xs) stack = do
    case runParser (parseCpt) x of
        Right (cpt, _) -> case cptToAst cpt of
            Right ast -> case preEvalAst ast env of
                Right ast_s -> case evalAst ast_s env of
                    Right result -> case (fst result) of
                        Empty -> someFuncFile (snd result) xs (createByteCode ast (snd result) (stack { bytecode = bytecode stack ++ [(show (codeLine stack)) ++ " "], codeLine = (codeLine stack) + 1 }))
                        _ -> case (end (createByteCode ast (snd result) (stack { bytecode = bytecode stack ++ [(show (codeLine stack)) ++ " "], codeLine = (codeLine stack) + 1 }))) of
                            True -> someFuncFile (snd result) [] (createByteCode ast (snd result) (stack { bytecode = bytecode stack ++ [(show (codeLine stack)) ++ " "], codeLine = (codeLine stack) + 1 }))
                            False -> someFuncFile (snd result) xs (createByteCode ast (snd result) (stack { bytecode = bytecode stack ++ [(show (codeLine stack)) ++ " "], codeLine = (codeLine stack) + 1 }))
                    Left err -> putStrLn("Error 1 : " ++ err) >> someFuncFile env xs (createByteCode ast env (stack { bytecode = bytecode stack ++ [(show (codeLine stack)) ++ " "], codeLine = (codeLine stack) + 1 }))
                Left err -> putStrLn("Error 2 : " ++ err) >> exitWith (ExitFailure 84)
            Left err -> putStrLn ("Error 3 : " ++ err) >> someFuncFile env xs (stack { bytecode = bytecode stack ++ [(show (codeLine stack)) ++ " "], codeLine = (codeLine stack) + 1 })
        Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)