module WangsAlgorithm.Parser where

import WangsAlgorithm.Proposition

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Char (isSpace)

parseProp :: Parser Proposition
parseProp = buildExpressionParser operators factor <?> "Proposition"
  where
    factor = do { _ <- char '(' ; x <- parseProp ; _ <- char ')' ; return x }
            <|> fmap Atom (many1 letter)
            <?> "Proposition"
    operators = [ unaries  Not     ["~", "-", "¬"]
                , binaries And     ["&", "^", "∧"]
                , binaries Or      ["|", "∨"]
                , binaries Implies ["->", "⊃", "→"] ]
    unary c n = Prefix . chainl1 (string n >> return c) $ return (.)
    binary c n = Infix (string n >> return c) AssocRight
    unaries c = map (unary c)
    binaries c = map (binary c)

-- | Parses prop list without the brackets.
parseProps :: Parser [Proposition]
parseProps = do {
    first <- parseProp
  ; next <- (char ',' >> parseProps) <|> return []
  ; return (first : next) } <|> return []

-- | Parses prop list with the brackets.
parsePropList :: Parser [Proposition]
parsePropList = do
    _ <- char '['
    l <- parseProps
    _ <- char ']'
    return l

parseSequent :: Parser Sequent
parseSequent  = do
  lefts <- parsePropList
  _ <- (char '|' >> char '-') <|> char '⊢'
  rights <- parsePropList
  return $ lefts `proves` rights

readSequent :: String -> Either ParseError Sequent
readSequent s = parse parseSequent "Sequent" (filter (not . isSpace) s)
