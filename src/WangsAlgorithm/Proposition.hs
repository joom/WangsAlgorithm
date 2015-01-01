module WangsAlgorithm.Proposition where

data Proposition = Atom   {name :: String}
                 | Not     Proposition
                 | And     Proposition Proposition
                 | Or      Proposition Proposition
                 | Implies Proposition Proposition
                   deriving Eq

data Sequent = Sequent [Proposition] [Proposition]
               deriving Eq

-- | A more readable infix alias for Sequent.
proves :: [Proposition] -> [Proposition] -> Sequent
proves = Sequent

instance Show Proposition where
  show (Atom    x)   = x
  show (Not     x)   = "¬(" ++ show x ++ ")"
  show (And     x y) = "(" ++ show x ++ ") ∧ (" ++ show y ++ ")"
  show (Or      x y) = "(" ++ show x ++ ") ∨ (" ++ show y ++ ")"
  show (Implies x y) = "(" ++ show x ++ ") ⊃ (" ++ show y ++ ")"

instance Show Sequent where
  show (Sequent x y) = show x ++ " ⊢ " ++ show y

notProp :: Proposition -> Proposition
notProp (Not x) = x
notProp y       = Not y

andProp :: Proposition -> (Proposition, Proposition)
andProp (And x y) = (x, y)
andProp _         = error "Only works on And."

orProp :: Proposition -> (Proposition, Proposition)
orProp (Or x y) = (x, y)
orProp _        = error "Only works on Or."

impProp :: Proposition -> (Proposition, Proposition)
impProp (Implies x y) = (x, y)
impProp _             = error "Only works on Implies."

isNot :: Proposition -> Bool
isNot (Not _) = True
isNot _       = False

isAnd :: Proposition -> Bool
isAnd (And _ _) = True
isAnd _         = False

isOr :: Proposition -> Bool
isOr (Or _ _) = True
isOr _        = False

isImp :: Proposition -> Bool
isImp (Implies _ _) = True
isImp _             = False
