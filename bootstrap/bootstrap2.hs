type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar c (x:xs) | c == x = Just (c, xs)
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar (x:xs) s = case parseChar x s of
  Just (c, s') -> Just (c, s')
  Nothing -> parseAnyChar xs s

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 s = case p1 s of
  Just (a, s') -> Just (a, s')
  Nothing -> case p2 s of
    Just (a, s') -> Just (a, s')
    Nothing -> Nothing

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 s = case p1 s of
  Just (a, s') -> case p2 s' of
    Just (b, s'') -> Just ((a, b), s'')
    Nothing -> Nothing
  Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 s = case parseAnd p1 p2 s of
  Just ((a, b), s') -> Just (f a b, s')
  Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p s = case p s of
  Just (a, s') -> case parseMany p s' of
    Just (as, s'') -> Just (a:as, s'')
    Nothing -> Just ([a], s')
  Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p s = case p s of
  (Just (a, s')) -> case parseMany p s' of
    (Just (as, s'')) -> Just (a:as, s'')
    Nothing -> Nothing
  Nothing -> Nothing

-- STEP 4