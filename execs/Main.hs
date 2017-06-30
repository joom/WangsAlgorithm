{-# LANGUAGE RecordWildCards #-}
module Main where

import WangsAlgorithm.Prover
import WangsAlgorithm.LaTeX (latexProof)
import qualified WangsAlgorithm.Parser as P
import Options.Applicative
import Data.Semigroup ((<>))

data Backend =
    Text
  | LaTeX
  deriving (Show, Eq, Read, Enum, Bounded)

allBackends :: [Backend]
allBackends = enumFrom minBound

data Input = Input
  { sequentStr :: String
  , backend    :: Backend
  }

getInput :: Parser Input
getInput = Input
  <$> strOption
       ( long "sequent"
      <> short 's'
      <> metavar "SEQUENT"
      <> help "The propositional logic sequent to be proved" )
  <*> option auto
       ( long "backend"
      <> short 'b'
      <> value Text
      <> help ("Select one of " ++ show allBackends))

run :: Input -> IO ()
run Input{..} = case P.readSequent sequentStr of
    Left err -> error $ "Cannot be parsed: " ++ show err
    Right sequent -> case prove sequent of
      Nothing -> error "No possible moves."
      Just pf -> case backend of
        Text -> putStrLn $ showProof pf
        LaTeX -> putStrLn $ latexProof pf

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (getInput <**> helper)
      ( fullDesc
     <> progDesc ("Enter your sequent in the following format: "
              ++ "[a|b, a&c, ~b, c->d] |- [b,c]")
     <> header "A propositional theorem prover for LK using Wang's Algorithm")
