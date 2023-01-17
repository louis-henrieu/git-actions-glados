import CPT
import AST

import Data.Char(digitToInt) -- very important

isDigit :: Char -> Bool -- check if a char is a number 
isDigit c = c >= '0' && c <= '9'

isNumber :: String -> Bool -- check if a word is a number
isNumber (c:s) = case (isDigit c, s) of
    (True, []) -> True
    (False, _) -> False
    (True, s) -> isNumber s
isNumber [] = False

concatIntArr :: [Int] -> Int -- correctly concatenates int numbers together
concatIntArr li = case li of
    [] -> 0
    x : [] -> x
    x : li -> (x * 10) + concatIntArr li

stringToInt :: String -> Int -- convert String to int
stringToInt s = concatIntArr (map digitToInt s)

parseType :: String -> Cpt -- attribute CPT type to each string
parseType (x) = if (isNumber x) then (Number (stringToInt x)) else (Symbol x)

parseWord :: String -> String -- from a string return the first word
parseWord [] = []
parseWord s = case s of
    '(' : s -> []
    ')' : s -> []
    ' ' : s -> []
    x : s -> x : (parseWord s)

goToNextWord :: String -> String -- get to the next word in the list
goToNextWord [] = ""
goToNextWord s = case s of
    ' ':s -> s
    '(':s -> s
    ')':s -> s
    x:s -> goToNextWord s 

goToClose :: String -> Int -> String -- get to the end of the as start parenthesis
goToClose [] _ = ""
goToClose s i = case (s, i) of
    (')':s, 0) -> s
    ('(':s, i) -> goToClose s (i + 1)
    (')':s, i) -> goToClose s (i - 1)
    (x:s, i) -> goToClose s i

parseCpt :: String -> [Cpt]
parseCpt s = case s of
    [] -> []
    '(' : s -> (List (parseCpt s)) : parseCpt (goToClose (s) 0)
    ')' : s -> []
    x:s -> (parseType (parseWord (x:s) )) : parseCpt (goToNextWord s)

main :: IO()
main = do
    line <- getLine
    case line of
        "quit" -> return ()
        _ -> do
            let cpt = List (parseCpt line)
            print (printTree cpt)
             
            print (cptToAST cpt)
    print line