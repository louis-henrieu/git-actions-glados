module Parser where

import Control.Applicative

data Cpt = Num Int | Node [Cpt] | Symbole String deriving (Show)

data Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap fct parser =
    Parser
      ( \s -> case runParser parser s of
          Right (a, s') -> Right (fct a, s')
          Left (x) -> Left (x)
      )

instance Applicative Parser where
  pure e = Parser (\s -> Right (e, s))
  pf <*> pa =
    Parser
      ( \s -> case runParser pf s of
          Left err -> Left err
          Right (f, rs) -> case runParser pa rs of
            Left err -> Left err
            Right (a, rs2) -> Right (f a, rs2)
      )

instance Alternative Parser where
  empty = Parser (\s -> Left ("Error empty"))
  pf <|> pa =
    Parser
      ( \s -> case runParser pf s of
          Left err -> case runParser pa s of
            Right res2 -> Right res2
            Left err2 -> Left (err ++ "\n" ++ err2)
          Right res1 -> Right res1
      )

parseChar :: Char -> Parser Char
parseChar c =
  Parser
    ( \s -> case s of
        (x : xs) | c == x -> Right (c, xs)
        _ -> Left ("Error ParseChar cannot find " ++ [c] ++ " in " ++ s)
    )

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser (\s -> Left ("Error ParseAnyChar"))
parseAnyChar (x : xs) =
  Parser
    ( \s -> case runParser (parseChar x) s of
        Right (c, s') -> Right (c, s')
        Left (x) -> runParser (parseAnyChar xs) s
    )

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 =
  Parser
    ( \s -> case runParser p1 s of
        Right (a, s') -> Right (a, s')
        Left (x) -> case runParser p2 s of
          Right (a, s') -> Right (a, s')
          Left (x) -> Left ("Error ParseOr")
    )

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 =
  Parser
    ( \s -> case runParser p1 s of
        Right (a, s') -> case runParser p2 s' of
          Right (b, s'') -> Right ((a, b), s'')
          Left (x) -> Left (x)
        Left (x) -> Left (x)
    )

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 =
  Parser
    ( \s -> case runParser p1 s of
        Right (a, s') -> case runParser p2 s' of
          Right (b, s'') -> Right ((f a b), s'')
          Left (x) -> Left (x)
        Left (x) -> Left (x)
    )

parseMany :: Parser a -> Parser [a]
parseMany p =
  Parser
    ( \s -> case runParser p s of
        Right (a, s') -> case runParser (parseMany p) s' of
          Right (as, s'') -> Right (a : as, s'')
          Left (x) -> Right ([a], s')
        Left (x) -> Right ([], s)
    )

parseSome :: Parser a -> Parser [a]
parseSome p =
  Parser
    ( \s -> case runParser p s of
        Right (a, s') -> case runParser (parseMany p) s' of
          Right (as, s'') -> Right (a : as, s'')
          Left (x) -> Left ("Error ParseSome")
        Left (x) -> Left ("Error ParseSome")
    )

parseUInt :: Parser Int
parseUInt =
  Parser
    ( \s -> case runParser (parseSome (parseAnyChar ['0' .. '9'])) s of
        Right (cs, s') -> Right (read cs, s')
        Left (x) -> Left ("Error ParseUInt")
    )

parseInt :: Parser Int
parseInt =
  Parser
    ( \s -> case runParser (parseChar '-') s of
        Right (c, s') -> case runParser (parseSome (parseAnyChar ['0' .. '9'])) s' of
          Right (cs, s'') -> Right (read (c : cs), s'')
          Left (x) -> Left ("Error ParseInt")
        Left (x) -> runParser parseUInt s
    )

-- parseString :: Parser String
-- parseString =
--   Parser
--     ( \s -> case runParser (parseMany (parseAnyChar ['a' .. 'z'])) s of
--         Right (cs, s') -> case cs of
--           "" -> Left ("Error ParseString")
--           _ -> case runParser (parseAnyChar ['a' .. 'z']) cs of
--             Right (c, s'') -> Right (cs, s')
--             Left (x) -> Left ("Error ParseString")
--         Left (x) -> Left ("Error ParseString")
--     )

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany (parseChar ' ')

isClosingParenthesis :: String -> Bool
isClosingParenthesis [] = False
isClosingParenthesis (x : xs) = case x of
  ')' -> True
  _ -> isClosingParenthesis xs

beforeClosingParenthesis :: String -> String -> String
beforeClosingParenthesis [] s = error "Error beforeClosingParenthesis"
beforeClosingParenthesis (x : xs) s = case x of
  ')' -> s
  _ -> beforeClosingParenthesis xs (s ++ [x])

afterClosingParenthesis :: String -> String
afterClosingParenthesis [] = error "Error afterClosingParenthesis"
afterClosingParenthesis (x : xs) = case x of
  ')' -> xs
  _ -> afterClosingParenthesis xs

parseList :: Parser a -> Parser [a]
parseList p =
  Parser
    ( \s -> case runParser (parseChar '(') s of
        Right (c, s') -> case runParser (parseMany (parseAndWith (,) p parseWhiteSpace)) s' of
          Right (as, s'') -> case runParser (parseChar ')') s'' of
            Right (c', s''') -> Right (map fst as, s''')
            Left (x) -> case isClosingParenthesis s'' of
              True -> case runParser p (beforeClosingParenthesis s'' "") of
                Right (a, s''') -> Right ((map fst as) ++ [a], afterClosingParenthesis s''')
                Left (x) -> Left ("Error ParseList - Invalid Parser argument")
              False -> Left ("Error ParseList - no closing parenthesis in : " ++ s'')
          Left (x') -> Left ("Error ParseList - Invalid Parser argument 2")
        Left (x) -> Left ("Error ParseList - no opening parenthesis in : \'" ++ s ++ "\'")
    )

-- parseList :: Parser a -> Parser [a]
-- parseList p = Parser ( \s -> case runParser (parseChar '(') s of
--         Right (c, s') -> case runParser (parseMany p) s' of
--           Right (as, s'') -> case runParser (parseChar ')') s'' of
--             Right (c, s''') -> Right (as, s''')
--             Left (x) -> Left ("Error ParseList")
--           Left (x) -> Left ("Error ParseList")
--         Left (x) -> Left ("Error ParseList")
--     )


-- parsing a list of elements separated by a separator


-- parseList :: Parser a -> Parser [a]
-- parseList p =
--   Parser
--     ( \s -> case runParser (parseChar '(') s of
--         Right (c, s') -> case runParser (parseManyWSepar ' ' p) s' of
--           Right (res, []) -> Left ("ERROR PARSE LIST: Missing closing char")
--           Right (res, ')':s) -> Right (res, s)
--           Right (res, '(':s) -> (runParser (joinParseList res)) ('(':s)
--           Right (res, x:s) -> (runParser (joinParseList res)) (x:s)
--           Left (x) -> Left ("Error ParseList")
--     )


-- joinParseList :: [Cpt] -> Parser Cpt
-- joinParseList prec = Parser (\s -> case (runParser parseList) s of
--   Right (res, ')':s) -> Right (List (prec ++ [res]), (s))
--   Right (res, s) -> Right (List (prec ++ [res]), (s))
--   Left (x) -> Left ("ERROR joinParseList" ++ x))

-- parseList :: Parser Cpt
-- parseList = Parser (\s -> case runParser (parseChar '(') s of
--   Right (c, s') -> case runParser (parseManyWSepar ' ' parseElem) (s') of
--     Right (res, []) -> Left ("ERROR PARSE LIST: Missing closing char")
--     Right (res, ')':s) -> Right (List res, s)
--     Right (res, '(':s) -> (runParser (joinParseList res)) ('(':s)
--     Right (res, x:s) -> Left ("ERROR PARSE LIST: Missing closing char" ++ x:s)
--     Left (x) -> Left ("ERROR: Parse List" ++ x)
--   Left (x) -> Left ("ERROR ParseList no case defined" ++ x))

-- parseElem :: Parser Cpt
-- parseElem = (Num <$> parseInt) <|> (Symbole <$> parseString) <|> (Symbole <$> parseSome (parseAnyChar "+-*/%"))
-- parseSeparChar :: String -> Char -> String
-- parseSeparchar s c = case s of
--   x:s x == c -> s
--   s -> s

--   -- parseManyWSepar is a parser that parse a list of elements separated by a char

-- parseManyWSepar :: Char -> Parser a -> Parser [a]
-- parseManyWSepar c p = Parser (\s -> case runParser p s of
--   Right (a, s') -> case runParser (parseManyWSepar c p) (parseSeparChar s' c) of
--     Right (as, s'') -> Right (a:as, s'')
--     Left(x) -> Right ([a], s')
--   Left(x) -> Right ([], s))

parseUntilNextSpace :: String -> Either String (String, String)
parseUntilNextSpace [] = Right ("", "")
parseUntilNextSpace (x : xs) = case x of
  ' ' -> Right ("", xs)
  _ -> case parseUntilNextSpace xs of
    Right (s, s') -> Right (x : s, s')
    Left (x) -> Left ("Error ParseUntilNextSpace")


parseString :: Parser String
parseString = parseSome (parseAnyChar ['a' .. 'z'] <|> parseAnyChar ['A' .. 'Z'] <|> parseAnyChar ['0' .. '9'] <|> parseAnyChar "+-*/%=<>")

parseCpt :: Parser Cpt
parseCpt = (Num <$> parseInt) <|> (Node <$> parseList parseCpt) <|> (Symbole <$> parseString)
  -- Parser
  --   ( \s -> case runParser (parseInt) s of
  --       Right (i, s') -> Right (Num i, s')
  --       Left (x) -> case runParser (Symbole <$> parseString) s of
  --         Right res -> Right res
  --         Left (x) -> case runParser (parseList parseCpt) s of
  --           Right (l, s') -> Right (Node l, s')
  --           Left (x) -> case parseUntilNextSpace s of
  --             Right (l, s') -> Right (Symbole l, s')
  --             Left (x) -> Left ("Error ParseCpt")
  --   )