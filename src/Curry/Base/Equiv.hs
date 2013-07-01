{- |
    Module      :  $Header$
    Description :  Type class for equivalence check
    Copyright   :  (c) 2013 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  stable
    Portability :  portable

    This module provides a simple type class to express the equivalence
    of two values. Unlike equality, the equivalence may abstract from
    certain properties like the order in a list.
-}
module Curry.Base.Equiv where

infix 4 =~=

-- |Type class to express the equivalence of two values
class Equiv a where
  -- |Test if two values are equivalent
  (=~=) :: a -> a -> Bool

instance Equiv a => Equiv (Maybe a) where
  Nothing =~= Nothing = True
  Just x  =~= Just y  = x =~= y
  _       =~= _       = False

instance Equiv a => Equiv [a] where
  []   =~= []   = True
  x:xs =~= y:ys = x =~= y && xs =~= ys
  _    =~= _    = False
