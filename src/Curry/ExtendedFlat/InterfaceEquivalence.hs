{- |
    Module      :  $Header$
    Description :  Check the equality of two FlatCurry interfaces
    Copyright   :  (c) 2006, Martin Engelke (men@informatik.uni-kiel.de)
                       2011, Björn Peemöller
                       2014, Jan Tikovsky
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}

module Curry.ExtendedFlat.InterfaceEquivalence (eqInterface) where

import Data.List (deleteFirstsBy)

import Curry.ExtendedFlat.Type

infix 4 =~=, `eqvSet`

-- |Check whether the interfaces of two FlatCurry programs are equal
eqInterface :: Prog -> Prog -> Bool
eqInterface = (=~=)

-- |Type class to express the equivalence of two values
class IntfEquiv a where
  (=~=) :: a -> a -> Bool

eqvSet :: IntfEquiv a => [a] -> [a] -> Bool
xs `eqvSet` ys = null (deleteFirstsBy (=~=) xs ys ++ deleteFirstsBy (=~=) ys xs)

instance IntfEquiv a => IntfEquiv [a] where
  []     =~= []     = True
  (x:xs) =~= (y:ys) = x =~= y && xs =~= ys
  _      =~= _      = False

instance IntfEquiv Char where
  (=~=) = (==)

instance IntfEquiv Prog where
  Prog m1 is1 ts1 fs1 os1 =~= Prog m2 is2 ts2 fs2 os2 =
    m1 == m2 && is1 `eqvSet` is2 && ts1 `eqvSet` ts2 && fs1 `eqvSet` fs2 && os1 `eqvSet` os2

instance IntfEquiv TypeDecl where
  Type qn1 vis1 tvs1 cs1 =~= Type qn2 vis2 tvs2 cs2 =
    qn1 == qn2 && vis1 == vis2 && tvs1 == tvs2 && cs1 =~= cs2
  TypeSyn qn1 vis1 tvs1 ty1 =~= TypeSyn qn2 vis2 tvs2 ty2 =
    qn1 == qn2 && vis1 == vis2 && tvs1 == tvs2 && ty1 == ty2
  _ =~= _ = False

instance IntfEquiv ConsDecl where
  Cons qn1 ar1 vis1 tys1 =~= Cons qn2 ar2 vis2 tys2 =
    qn1 == qn2 && ar1 == ar2 && vis1 == vis2 && tys1 == tys2

instance IntfEquiv FuncDecl where
  Func qn1 ar1 vis1 ty1 r1 =~= Func qn2 ar2 vis2 ty2 r2 =
    qn1 == qn2 && ar1 == ar2 && vis1 == vis2 && ty1 == ty2 && r1 =~= r2

-- TODO: check why arguments of rules are not checked for equivalence
instance IntfEquiv Rule where
  Rule _ _   =~= Rule _ _   = True
  External _ =~= External _ = True
  _          =~= _          = False

instance IntfEquiv OpDecl where
  Op qn1 fix1 p1 =~= Op qn2 fix2 p2 =
    qn1 == qn2 && fix1 == fix2 && p1 == p2
