{-# LANGUAGE OverloadedStrings #-}
module WangsAlgorithm.LaTeX where

import WangsAlgorithm.Proposition
import WangsAlgorithm.Prover

import Data.List (intersperse)
import Data.Maybe
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.AMSMath

ruleName :: Rule -> LaTeX
ruleName r = math $ case r of
  Id             -> "I"
  WeakeningLeft  -> "WL"
  WeakeningRight -> "WR"
  NotLeft        -> comm0 "lnot"   <> "L"
  NotRight       -> comm0 "lnot"   <> "R"
  AndLeft        -> comm0 "wedge"  <> "L"
  AndRight       -> comm0 "wedge"  <> "R"
  OrLeft         -> comm0 "vee"    <> "L"
  OrRight        -> comm0 "vee"    <> "R"
  ImpliesLeft    -> comm0 "supset" <> "L"
  ImpliesRight   -> comm0 "supset" <> "R"

prop :: Proposition -> LaTeX
prop (Atom    x)   = fromString x
prop (Not     x)   = comm0 "lnot" <> autoParens (prop x)
prop (And     x y) = autoParens (prop x `wedge`  prop y)
prop (Or      x y) = autoParens (prop x `vee`    prop y)
prop (Implies x y) = autoParens (prop x `supset` prop y)

sequent :: Sequent -> LaTeX
sequent (Sequent x y) =
  math $ mconcat $  intersperse "," (map prop x) ++ [comm0 "vdash"]
                 ++ intersperse "," (map prop y)

proof :: Proof -> LaTeX
proof (Linear (ProofStep r bf) n) =
  (if isNothing n then comm1 "AxiomC" mempty else mempty) <>
  maybe mempty proof n <>
  comm1 "RightLabel" (comm0 "scriptsize" <> ruleName r) <>
  comm1 "UnaryInfC" (sequent bf)
proof (Branch (ProofStep r bf) p1 p2) =
  maybe mempty proof p1 <>
  maybe mempty proof p2 <>
  comm1 "RightLabel" (comm0 "scriptsize" <> ruleName r) <>
  comm1 "BinaryInfC" (sequent bf)

entireProof :: Proof -> LaTeX
entireProof = TeXEnv "prooftree" [] . proof

latexProof :: Proof -> String
latexProof = prettyLaTeX . entireProof
