module WangsAlgorithm.Prover where

import Data.List (partition)
import WangsAlgorithm.Proposition

data Rule = Id
          | IdStar
          | NotLeft
          | NotRight
          | AndLeft
          | AndRight
          | OrLeft
          | OrRight
          | ImpliesLeft
          | ImpliesRight
          deriving (Show, Eq)

data ProofStep = ProofStep { rule   :: Rule
                           , before :: Sequent
                           } deriving (Show, Eq)

data Proof = Linear { step   :: ProofStep
                    , next   :: Maybe Proof }
           | Branch { step   :: ProofStep
                    , first  :: Maybe Proof
                    , second :: Maybe Proof
                    } deriving Eq

solver :: Sequent -> Maybe Proof
solver sequent@(Sequent lefts rights)
  | any (`elem` rights) lefts =
      Just $ Linear (ProofStep IdStar sequent) Nothing

  -- If one of the formulae separated by commas is the negation of a
  -- formula, drop the negation sign and move it to the other side of the
  -- arrow.
  | any isNot lefts =
      let new = leftsWithoutNot `proves` (rights ++ map notProp leftsWithNot)
      in Just $ Linear (ProofStep NotLeft sequent) (solver new)
  | any isNot rights =
      let new = (lefts ++ map notProp rightsWithNot) `proves` rightsWithoutNot
      in Just $ Linear (ProofStep NotRight sequent) (solver new)

  -- If the principal connective of a formula on the left is ^ (and), or on
  -- the right of the arrow is v (or), replace the connective by a comma.
  | any isAnd lefts =
      let new = (leftsWithoutAnd ++ concatMap (toList . andProp) leftsWithAnd)
                `proves` rights
      in Just $ Linear (ProofStep AndLeft sequent) (solver new)

  | any isOr rights =
      let new = lefts `proves`
                (rightsWithoutOr ++ concatMap (toList . orProp) rightsWithOr)
      in Just $ Linear (ProofStep OrRight sequent) (solver new)

  | any isImp rights =
      let new = (lefts ++ map (fst . impProp) rightsWithImp) `proves`
                (rightsWithoutImp ++ map (snd . impProp) rightsWithImp)
      in Just $ Linear (ProofStep ImpliesRight sequent) (solver new)

  -- If the principal connective of a formula on the left is v (or), or on
  -- the right of the arrow is ^ (and), then produce two new lines, each
  -- with one of the two sub-formulae replacing the formula. Both of these
  -- must be proved in order to prove the original theorem.
  | any isOr lefts   =
      let (x:_)    = leftsWithOr in
      let (p1, p2) = orProp x    in
      let new1 = (p1 : filter (/= x) lefts) `proves` rights in
      let new2 = (p2 : filter (/= x) lefts) `proves` rights in
      Just $ Branch (ProofStep OrLeft sequent)
                    (solver new1) (solver new2)
  | any isAnd rights   =
      let (x:_)    = rightsWithAnd in
      let (p1, p2) = andProp x     in
      let new1 = lefts `proves` (p1 : filter (/= x) rights) in
      let new2 = lefts `proves` (p2 : filter (/= x) rights) in
      Just $ Branch (ProofStep AndRight sequent)
                    (solver new1) (solver new2)

  | any isImp lefts   =
      let (x:_)    = leftsWithImp in
      let (p1, p2) = impProp x    in
      let new1 = filter (/= x) lefts `proves` (p1 : rights) in
      let new2 = (p2 : filter (/= x) lefts) `proves` rights in
      Just $ Branch (ProofStep ImpliesLeft sequent)
                    (solver new1) (solver new2)

  | otherwise       = Nothing
      where (leftsWithNot,  leftsWithoutNot)  = partition isNot lefts
            (leftsWithAnd,  leftsWithoutAnd)  = partition isAnd lefts
            (leftsWithOr,   _)                = partition isOr  lefts
            (leftsWithImp,  _)                = partition isImp lefts

            (rightsWithNot, rightsWithoutNot) = partition isNot rights
            (rightsWithAnd, _)                = partition isAnd rights
            (rightsWithOr,  rightsWithoutOr)  = partition isOr  rights
            (rightsWithImp, rightsWithoutImp) = partition isImp rights

tab :: String -> String
tab = unlines . map ("    "++) . lines

instance Show Proof where
  show (Linear (ProofStep r bf) n) = init $ unlines [
      "Before: " ++ show bf,
      "Rule: "   ++ show r,
      "-------------------",
      rest ]
    where rest = case n of
                   Just proof -> show proof
                   _          -> "End."
  show (Branch (ProofStep r bf) p1 p2) = init $ unlines [
      "Before: " ++ show bf,
      "Rule: "   ++ show r,
      "-------------------",
      "First branch: ",
      tab (rest p1),
      "-------------------",
      "Second branch: ",
      tab (rest p2),
      "-------------------" ]
    where rest p = case p of
                    Just proof -> show proof
                    _          -> "End."

completeProof :: Proof -> Bool
completeProof (Linear (ProofStep r _) n) =
    r `elem` [Id,IdStar] || rest
  where rest = case n of
                 Just x -> completeProof x
                 _      -> False
completeProof (Branch (ProofStep _ _) p1 p2) =
    rest p1 && rest p2
  where rest p = case p of
                   Just x -> completeProof x
                   _      -> False
