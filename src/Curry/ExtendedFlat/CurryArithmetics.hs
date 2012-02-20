{- |
    Module      :  $Header$
    Description :  Representation of Integer as ADTs
    Copyright   :  (c) 2009, Holger Siegel
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

-}
module Curry.ExtendedFlat.CurryArithmetics
  ( CurryInt (..), CurryNat (..), trNat, trInt, toCurryInt, toIntExpression
  ) where

import Curry.ExtendedFlat.Type

-- |Data type for curry's 'Int' representation
data CurryInt
  = Neg CurryNat -- ^ negative integer
  | Zero         -- ^ zero
  | Pos CurryNat -- ^ positive integer

-- |Data type for curry's representation of natural numbers
data CurryNat
  = IHi         -- ^ highest one bit
  | O CurryNat  -- ^ zero bit
  | I CurryNat  -- ^ one bit

-- |Translate a natural number into its algebraic representation,
-- providing functions for representing the highest bit, a zero bit and
-- a one bit.
trNat :: Integral n => a -> (a -> a) -> (a -> a) -> n -> a
trNat h o i = go
    where go n | n `mod` 2 == 0 = o (go m)
               | m == 0         = h
               | otherwise      = i (go m)
              where m = n `div` 2

-- |Translate an 'Integral' number into its algebraic representation,
-- providing functions for representing negative numbers, zero, positive
-- numbers, highest bit, a zero bit and a one bit.
trInt :: Integral n =>
         (nat -> t) -> t -> (nat -> t) ->
         nat -> (nat -> nat) -> (nat -> nat) ->
         n -> t
trInt n z p h o i = go
    where go x = case compare x 0 of
                   LT -> n (trNat h o i (negate x))
                   EQ -> z
                   GT -> p (trNat h o i x)

-- |Convert an 'Integral' value into its algebraic representation.
toCurryInt :: Integral a => a -> CurryInt
toCurryInt = trInt Neg Zero Pos IHi O I

-- |Convert an 'Integral' value into an expression constructing
-- its algebraic representation.
toIntExpression :: Integral a => a -> Expr
toIntExpression = trInt neg_ zero_ pos_ iHi_ o_ i_

-- |Construct a 'TypeExpr' in the Prelude for a given type name
prelType :: String -> TypeExpr
prelType s = TCons (mkQName ("Prelude", s)) []

-- |Construct a type constructor in the Prelude for a given type name
prelCons :: TypeExpr -> String -> [Expr] -> Expr
prelCons t = Comb ConsCall . QName Nothing (Just t) "Prelude"

tInt0 :: TypeExpr
tInt0 = prelType "Int"

tInt1 :: TypeExpr
tInt1 = FuncType tInt0 tInt0

tNat0 :: TypeExpr
tNat0 = prelType "Nat"

tNat1 :: TypeExpr
tNat1 = FuncType tNat0 tNat0

zero_ :: Expr
zero_  = prelCons tInt0 "Zero" []

iHi_ :: Expr
iHi_ = prelCons tNat0 "IHi" []

pos_ :: Expr -> Expr
pos_ n = prelCons tInt1 "Pos" [n]

neg_ :: Expr -> Expr
neg_ n = prelCons tInt1 "Neg" [n]

o_ :: Expr -> Expr
o_ n = prelCons tNat1 "O" [n]

i_ :: Expr -> Expr
i_ n = prelCons tNat1 "I" [n]
