module Lib
    (
        someFuncGetLine,
        someFuncFile
    ) where

import Ast
import Env
import Info
import Keywords
import Parser
import System.Exit
import System.IO (isEOF)

printAst :: Ast -> IO ()
printAst ast = case ast of
    (IntegerAst i) -> putStrLn (show i)
    (SymbolAst s) -> putStrLn s
    (FloatAst f) -> if f == (fromIntegral (round f))
        then
            putStrLn (show (round f))
        else
            putStrLn (show f)
    (Lambda x ast2) -> print "#<procedure>"
    _ -> print "The Ast isn't valid"

evalAll :: [Ast] -> Env -> IO ()
evalAll [] env = return ()
evalAll (x:xs) env =
    case preEvalAst x env of
        Right ast -> case evalAst ast env of
            Right result -> case (fst result) of
                Empty -> evalAll xs (snd result)
                _ -> printAst (fst result) >> evalAll xs (snd result)
            Left err -> print (err)
        Left err -> print (err)


evalFunc :: Ast -> Env -> IO ()
evalFunc ast env = do
    case preEvalAst ast env of
        Right ast -> case evalAst ast env of
            Right result -> case (fst result) of
                Empty -> putStrLn "ok"
                _ -> printAst (fst result)
            Left err -> putStrLn("Error : " ++ err)
        Left err -> putStrLn("Error : " ++ err)


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
                        Right ast -> case evalAst ast env of
                            Right result -> case (fst result) of
                                Empty -> someFuncGetLine (snd result)
                                _ -> printAst (fst result) >> someFuncGetLine (snd result)
                            Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                        Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                    Left err -> putStrLn ("Error : " ++ err) >> someFuncGetLine env
                Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)

someFuncFile :: Env -> [String] -> IO()
someFuncFile env [] = return ()
someFuncFile env (x:xs) = do
    case runParser (parseCpt) x of
        Right (cpt, _) -> case cptToAst cpt of
            Right ast -> case preEvalAst ast env of
                Right ast -> case evalAst ast env of
                    Right result -> case (fst result) of
                        Empty -> someFuncFile (snd result) xs
                        _ -> printAst (fst result) >> someFuncFile (snd result) xs
                    Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
                Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)
            Left err -> putStrLn ("Error : " ++ err) >> someFuncFile env xs
        Left err -> putStrLn("Error : " ++ err) >> exitWith (ExitFailure 84)