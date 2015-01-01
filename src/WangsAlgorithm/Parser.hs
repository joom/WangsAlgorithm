module WangsAlgorithm.Parser where

import WangsAlgorithm.Proposition

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Char (isSpace)

expr :: Parser Proposition
expr = buildExpressionParser operators factor <?> "Proposition"
  where
    factor = do { _ <- char '(' ; x <- expr ; _ <- char ')' ; return x }
            <|> atom
            <?> "Proposition"
    operators = [ unaries  Not     ["~", "-", "¬"]
                , binaries And     ["&", "^", "∧"]
                , binaries Or      ["|", "∨"]
                , binaries Implies ["->", "⊃", "→"] ]
    unary c n = Prefix . chainl1 (string n >> return c) $ return (.)
    binary c n = Infix (string n >> return c) AssocRight
    unaries c = map (unary c)
    binaries c = map (binary c)

atom :: Parser Proposition
atom = do { ds <- many1 letter ; return (Atom ds)} <?> "Atom"

props :: Parser [Proposition]
props = do {
    first <- expr
  ; next <- (char ',' >> props) <|> return []
  ; return (first : next) } <|> return []

propList :: Parser [Proposition]
propList = do
    _ <- char '['
    l <- props
    _ <- char ']'
    return l

sequent :: Parser Sequent
sequent  = do
  lefts <- propList
  _ <- (char '|' >> char '-') <|> char '⊢'
  rights <- propList
  return $ lefts `proves` rights

readSequent :: String -> Either ParseError Sequent
readSequent s = parse sequent "Sequent" (filter (not . isSpace) s)
