data Parser a = Parser {
  runParser :: String -> Either String (a, String)
}

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  pf <*> pa = Parser (\s -> case runParser pf s of
    Left err -> Left err
    Right (f,rs) -> case runParser pa rs of
      Left err -> Left err
      Right (a,rs2) -> Right (f a, rs2))
  
class Functor f2 => Alternative f2 where
  pure2 :: a -> f2 a
  (<|>) :: f2 a -> f2 a -> f2 a
  pf <|> pa = Parser (\s -> case runParser pf s of
    Left err -> case runParser pa s of
      Right (a, rs) -> Right (a, rs)
      Left err -> (\s -> case (runParser pa s) of
        Left err2 -> Left (err ++ "\n" ++ err2)
        Right (a, rs) -> Right (a, rs))
    Right (f, rs) -> Right (f, rs))

instance Functor Parser where
  fmap f p = Parser (\s -> case runParser p s of
    Right (a, s') -> Right (f a, s')
    Left(x) -> Left(x))

parseChar :: Char -> Parser Char
parseChar c = Parser (\s -> case s of
  (x:xs) | c == x -> Right (c, xs)
  _ -> Left ("Error ParseChar cannot find " ++ [c] ++ " in " ++ s))

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser (\s -> Left ("Error ParseAnyChar"))
parseAnyChar (x:xs) = Parser (\s -> case runParser (parseChar x) s of
  Right (c, s') -> Right (c, s')
  Left(x) -> runParser (parseAnyChar xs) s)


parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser (\s -> case runParser p1 s of
  Right (a, s') -> Right (a, s')
  Left(x) -> case runParser p2 s of
    Right(a, s') -> Right (a, s')
    Left(x) -> Left("Error ParseOr"))

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser (\s -> case runParser p1 s of
  Right (a, s') -> case runParser p2 s' of
    Right (b, s'') -> Right ((a, b), s'')
    Left(x) -> Left(x)
  Left(x) -> Left(x))

parseAndWith:: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser (\s -> case runParser p1 s of
  Right (a, s') -> case runParser p2 s' of
    Right (b, s'') -> Right ((f a b), s'')
    Left(x) -> Left(x)
  Left(x) -> Left(x))

parseMany :: Parser a -> Parser [a]
parseMany p = Parser (\s -> case runParser p s of
  Right (a, s') -> case runParser (parseMany p) s' of
    Right (as, s'') -> Right (a:as, s'')
    Left(x) -> Right ([a], s')
  Left(x) -> Right ([], s))

parseSome :: Parser a -> Parser [a]
parseSome p = Parser (\s -> case runParser p s of
  Right (a, s') -> case runParser (parseMany p) s' of
    Right (as, s'') -> Right (a:as, s'')
    Left(x) -> Left("Error ParseSome")
  Left(x) -> Left("Error ParseSome"))

parseUInt :: Parser Int
parseUInt = Parser (\s -> case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
  Right (cs, s') -> Right (read cs, s')
  Left(x) -> Left("Error ParseUInt"))


parseInt :: Parser Int
parseInt = Parser (\s -> case runParser (parseChar '-') s of
  Right (c, s') -> case runParser (parseSome (parseAnyChar ['0'..'9'])) s' of
    Right (cs, s'') -> Right (read (c:cs), s'')
    Left(x) -> Left("Error ParseInt")
  Left(x) -> runParser parseUInt s)

parseString :: Parser String
parseString = Parser (\s -> case runParser (parseChar '"') s of
  Right (c, s') -> case runParser (parseMany (parseAnyChar ['a'..'z'])) s' of
    Right (cs, s'') -> case runParser (parseChar '"') s'' of
      Right (c', s''') -> Right (cs, s''')
      Left(x) -> Left("Error ParseString")
    Left(x) -> Left("Error ParseString")
  Left(x) -> Left("Error ParseString"))

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany (parseChar ' ')

isClosingParenthesis :: String -> Bool
isClosingParenthesis [] = False
isClosingParenthesis (x:xs) = case x of
  ')' -> True
  _ -> isClosingParenthesis xs

beforeClosingParenthesis :: String -> String -> String
beforeClosingParenthesis [] s = error "Error beforeClosingParenthesis"
beforeClosingParenthesis (x:xs) s = case x of
  ')' -> s
  _ -> beforeClosingParenthesis xs (s ++ [x])

afterClosingParenthesis :: String -> String
afterClosingParenthesis [] = error "Error afterClosingParenthesis"
afterClosingParenthesis (x:xs) = case x of
  ')' -> xs
  _ -> afterClosingParenthesis xs

parseList :: Parser a -> Parser [a]
parseList p = Parser (\s -> case runParser (parseChar '(') s of
  Right (c, s') -> case runParser (parseMany (parseAndWith (,) p parseWhiteSpace)) s' of
    Right (as, s'') -> case runParser (parseChar ')') s'' of
      Right (c', s''') -> Right (map fst as, s''')
      Left(x) -> case isClosingParenthesis s'' of
        True -> case runParser p (beforeClosingParenthesis s'' "") of
          Right(a, s''') -> Right ((map fst as) ++ [a], afterClosingParenthesis s'')
          Left(x) -> Left ("Error ParseList - Invalid Parser argument")
        False -> Left("Error ParseList - no closing parenthesis in : " ++ s'')
    Left(x') -> Left ("Error ParseList - Invalid Parser argument")
  Left(x) -> Left ("Error ParseList - no opening parenthesis in : \'" ++ s ++ "\'"))
