module Main where

import WangsAlgorithm.Parser
import WangsAlgorithm.Proposition
import WangsAlgorithm.Prover

import Data.Maybe (isJust)
import Control.Monad
import Control.Applicative

import Test.HUnit
import Test.QuickCheck

maybeReadSequent :: String -> Maybe Sequent
maybeReadSequent s = case readSequent s of
  Left  _ -> Nothing
  Right x -> Just x

example :: Sequent
example =  Sequent [ Or (Atom "a") (Atom "b"), And (Atom "a") (Atom "c"),
                     Not (Atom "b"), Implies (Atom "c") (Atom "d") ]
                   [ Atom "b", Atom "c", And (Atom "c") (Atom "d") ]

-- these examples should all have a complete proof
tautologyExamples :: [(String, String)]
tautologyExamples =
  [ ("Modus Ponens", "[(p->q)&p] |- [q]")
  , ("Modus Tollens", "[(p->q)&(~q)] |- [~p]")
  , ("Hypothetical Syllogism", "[((p->q)&(q->r))] |- [p->r]")
  , ("Disjunctive Syllogism", "[(p|q)&(~p)] |- [q]")
  , ("Constructive Dilemma", "[(p->q)&(r->s)&(p|r)] |- [q|s]")
  , ("Destructive Dilemma", "[(p->q)&(r->s)&((~q)|(~s))] |- [(~p)|(~r)]")
  , ("Bidirectional Dilemma", "[(p->q)&(r->s)&(p|(~s))] |- [q|(~r)]")
  , ("Simplification", "[p&q] |- [p]")
  , ("Conjunction", "[p,q] |- [p&q]")
  , ("Addition", "[p] |- [p|q]")
  , ("Composition", "[(p->q)&(p->r)] |- [p->(q&r)]")
  , ("De Morgan's Theorem (1)", "[~(p&q)] |- [(~p)|(~q)]")
  , ("De Morgan's Theorem (2)", "[~(p|q)] |- [(~p)&(~q)]")
  , ("Commutation (1)", "[p|q] |- [q|p]")
  , ("Commutation (2)", "[p&q] |- [q&p]")
  , ("Association (1)", "[p|(q|r)] |- [(p|q)|r]")
  , ("Association (2)", "[p&(q&r)] |- [(p&q)&r]")
  , ("Distribution (1)", "[p&(q|r)] |- [(p&q)|(p&r)]")
  , ("Distribution (2)", "[p|(q&r)] |- [(p|q)&(p|r)]")
  , ("Double Negation", "[p] |- [~(~p)]")
  , ("Transposition", "[p->q] |- [(~q)->(~p)]")
  , ("Material Implication", "[p->q] |- [(~p)|q]")
  , ("Exportation", "[(p&q)->r] |- [p->(q->r)]")
  , ("Importation", "[p->(q->r)] |- [(p&q)->r]")
  , ("Tautology (1)", "[p] |- [p|p]")
  , ("Tautology (2)", "[p] |- [p&p]")
  , ("Law of Excluded Middle", "[] |- [p|(~p)]")
  , ("Law of Non-Contradiction", "[] |- [~(p&(~p))]")
  ]

-- HUnit tests
tautologyTests :: [Assertion]
tautologyTests =
  map (\(s,t) -> assertEqual s (Just True)
                 (completeProof <$> (solver =<< maybeReadSequent t)))
      tautologyExamples

assertExample :: String -> Assertion
assertExample s = assertEqual s (Just example) (maybeReadSequent s)

tests :: Test
tests = TestList $ map TestCase $
  [
    -- mostly parsing tests
    -- empty list
    assertEqual "[] |- []" (Just $ Sequent [] []) (maybeReadSequent "[] |- []")
    -- singleton
  , assertEqual "[a] |- [a]"
    (Just $ Sequent [Atom "a"] [Atom "a"])
    (maybeReadSequent "[a] |- [a]")
    -- multiple atomic
  , assertEqual "[a,b] |- [a,c]"
    (Just $ Sequent [Atom "a", Atom "b"] [Atom "a", Atom "c"])
    (maybeReadSequent "[a,b] |- [a,c]")
    -- multiple connectives
  , assertExample "[a|b, a&c, ~b, c->d] |- [b, c, c&d]"
    -- with spaces
  , assertExample "[a | b, a & c, ~b, c -> d] |- [b, c, c & d]"
    -- fancy characters
  , assertExample "[(a)∨(b),(a)∧(c),¬(b),(c)⊃(d)]⊢[b,c,(c)∧(d)]"
    -- fancy characters with spaces
  , assertExample "[(a) ∨ (b),(a) ∧ (c),¬(b),(c) ⊃ (d)] ⊢ [b,c,(c) ∧ (d)]"
  ] ++ tautologyTests

-- QuickCheck tests

instance Arbitrary Proposition where
  arbitrary = oneof [ liftM  Atom    arbitrary
                    , liftM  Not     arbitrary
                    , liftM2 And     arbitrary arbitrary
                    , liftM2 Or      arbitrary arbitrary
                    , liftM2 Implies arbitrary arbitrary ]

instance Arbitrary Sequent where
  arbitrary = liftM2 Sequent arbitrary arbitrary

parseable :: Sequent -> Bool
parseable = isJust . maybeReadSequent . show

-- TODO: more QuickCheck tests

-- General

runTests ::  IO ()
runTests = do
  _ <- runTestTT tests
  -- mapM_ quickCheck [parseable]
  return ()

-- | For now, main will run our tests.
main :: IO ()
main = runTests
