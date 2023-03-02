module Parser (
  parseCpt,
  runParser,
) where

import Control.Applicative

import Cpt

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
  empty = Parser (\_ -> Left ("Error empty"))
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
parseAnyChar [] = Parser (\_ -> Left ("Error ParseAnyChar"))
parseAnyChar (x : xs) =
  Parser
    ( \s -> case runParser (parseChar x) s of
        Right (c, s') -> Right (c, s')
        Left (_) -> runParser (parseAnyChar xs) s
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
          Left (_) -> Right ([a], s')
        Left (_) -> Right ([], s)
    )

parseSome :: Parser a -> Parser [a]
parseSome p =
  Parser
    ( \s -> case runParser p s of
        Right (a, s') -> case runParser (parseMany p) s' of
          Right (as, s'') -> Right (a : as, s'')
          Left (_) -> Left ("Error ParseSome")
        Left (_) -> Left ("Error ParseSome")
    )

parseUFloat :: Parser Float
parseUFloat =
  Parser
    ( \s -> case runParser (parseSome (parseAnyChar ['0' .. '9'])) s of
        Right (cs, s') -> case runParser (parseChar '.') s' of
          Right (c, s'') -> case runParser (parseSome (parseAnyChar ['0' .. '9'])) s'' of
            Right (cs', s''') -> Right (read (cs ++ [c] ++ cs'), s''')
            Left (_) -> Left ("Error ParseUFloat")
          Left (_) -> Right (read cs, s')
        Left (_) -> Left ("Error ParseUFloat")
    )
  
parseFloat :: Parser Float
parseFloat =
  Parser 
    ( \s  -> case runParser (parseChar '-') s of
      Right (_, s') -> case runParser (parseUFloat) s' of
        Right (cs, s'') -> Right (cs * (-1), s'')
        Left (x) -> Left (x)
      Left (_) -> case runParser (parseUFloat) s of
        Right x -> Right x
        Left (x) -> Left (x)
    )

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany (parseChar ' ')

isClosingParenthesis :: String -> Bool
isClosingParenthesis [] = False
isClosingParenthesis (x : xs) = case x of
  ')' -> True
  _ -> isClosingParenthesis xs

beforeClosingParenthesis :: String -> String -> String
beforeClosingParenthesis [] _ = error "Error beforeClosingParenthesis"
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
        Right (_, s') -> case runParser (parseMany (parseAndWith (,) p parseWhiteSpace)) s' of
          Right (as, s'') -> case runParser (parseChar ')') s'' of
            Right (_, s''') -> Right (map fst as, s''')
            Left (_) -> case isClosingParenthesis s'' of
              True -> case runParser p (beforeClosingParenthesis s'' "") of
                Right (a, s''') -> Right ((map fst as) ++ [a], afterClosingParenthesis s''')
                Left (_) -> Left ("Error ParseList - Invalid Parser argument")
              False -> Left ("Error ParseList - no closing parenthesis in : " ++ s'')
          Left (_) -> Left ("Error ParseList - Invalid Parser argument 2")
        Left (_) -> Left ("Error ParseList - no opening parenthesis in : \'" ++ s ++ "\'")
    )

parseString :: Parser String
parseString = parseSome (parseAnyChar ['a' .. 'z'] <|>  parseAnyChar ['A' .. 'Z'] <|> parseAnyChar ['0' .. '9'] <|> parseAnyChar "?!+-*/%=<>#")

parseCpt :: Parser Cpt
parseCpt = (NumberFloat <$> parseFloat) <|> (List <$> parseList parseCpt) <|> (Symbol <$> parseString)