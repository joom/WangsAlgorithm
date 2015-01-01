module Main where

import System.IO
import WangsAlgorithm.Prover
import WangsAlgorithm.Proposition
import qualified WangsAlgorithm.Parser as P
import qualified Text.ParserCombinators.Parsec as P

getSequent :: IO (Either P.ParseError Sequent)
getSequent = do
    putStr "?: "
    hFlush stdout
    input <- getLine
    return $ P.readSequent input

-- | Starts the REPL session.
replStart :: IO ()
replStart = do
  sequent <- getSequent
  case sequent of
    Left err -> putStrLn "Cannot be parsed: " >> print err
    Right sq -> case solver sq of
                  Just proof -> do print proof
                                   putStrLn $ if completeProof proof
                                              then "Proof completed.\n"
                                              else "This cannot be proved.\n"
                  _          -> putStrLn "No possible moves."
  replStart

main :: IO ()
main = do
  (putStrLn . unlines)
    [ "A Propositional Theorem Prover using Wang's Algorithm"
    , "Joomy Korkut - http://github.com/joom/WangsAlgorithm"
    , " -- Enter your sequent in this format:"
    , " -- [a|b, a&c, ~b, c->d] |- [b, c, c&d]"]
  replStart
