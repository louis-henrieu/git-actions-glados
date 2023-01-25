type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar c (x:xs) | c == x = Right (c, xs)
parseChar _ _ = Left ("Error ParseChar")

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Left ("!ParseAnyChar")
parseAnyChar (x:xs) s = case parseChar x s of
  Right (c, s') -> Right (c, s')
  Left(x) -> parseAnyChar xs s

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 s = case p1 s of
  Right (a, s') -> Right (a, s')
  Left(x) -> case p2 s of
    Right(a, s') -> Right (a, s')
    Left(x) -> Left("Error ParseOr")

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 s = case p1 s of
  Right (a, s') -> case p2 s' of
    Right (b, s'') -> Right ((a, b), s'')
    Left(x) -> Left("Error ParseAnd") 
  Left(x) -> Left("Error ParseAnd")

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 s = case parseAnd p1 p2 s of
  Right ((a, b), s') -> Right (f a b, s')
  Left (x) -> Left("Error ParseAndWith")

parseMany :: Parser a -> Parser [a]
parseMany p s = case p s of
  Right (a, s') -> case parseMany p s' of
    Right (as, s'') -> Right (a:as, s'')
    Left(x) -> Right ([a], s')
  Left(x) -> Right ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p s = case p s of
  (Right (a, s')) -> case parseMany p s' of
    (Right (as, s'')) -> Right (a:as, s'')
    Left(x) -> Left("Error ParseSome")
  Left(x) -> Left("Error ParseSome")

parseUInt :: Parser Int
parseUInt s = case parseSome (parseAnyChar ['0'..'9']) s of
  Right (cs, s') -> Right (read cs, s')
  Left(x) -> Left("Error ParseUInt")


parseInt :: Parser Int
parseInt s = case (parseChar '-' s) of
  Right (c, s') -> case (parseSome (parseAnyChar ['0'..'9']) s') of
    Right (cs, s'') -> Right (read (c:cs), s'')
    Left(x) -> Left("Error ParseInt")
  Left(x) -> parseUInt s

parsePair :: Parser a -> Parser (a, a)
parsePair p (s:s') = case s of
  '(' -> case (p s') of
    Right (x, e:s'') -> case e of
      ' ' -> case p s'' of
        Right (y, e':s''') -> case e' of
          ')' -> Right ((x,y), s''')
          _ -> Left ("Error ParsePair")
        Left(x) -> Left ("Error ParsePair")
      _ -> Left ("Error ParsePair")
    Left(x) -> Left ("Error ParsePair") 
  _ -> Left ("Error ParsePair")

parseWhiteSpace :: Parser String
parseWhiteSpace s = parseSome (parseChar ' ') s

parseList :: Parser a -> Parser [a]
parseList p s = case parseChar '(' s of
  Right (c, s') -> case parseMany (parseAndWith (,) p parseWhiteSpace) s' of
    Right (as, s'') -> Right (map fst as, s'')
    Left(x) -> Left ("Error ParseList")
  Left(x) -> Left ("Error ParseList")