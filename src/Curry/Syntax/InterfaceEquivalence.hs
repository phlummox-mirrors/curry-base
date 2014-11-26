{- |
    Module      :  $Header$
    Description :  Comparison of Curry Interfaces
    Copyright   :  (c) 2000 - 2007, Wolfgang Lux
                       2014       , Björn Peemöller
                       2014       , Jan Tikovsky
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    If a module is recompiled, the compiler has to check whether the
    interface file must be updated. This must be done if any exported
    entity has been changed, or an export was removed or added. The
    function 'intfEquiv' checks whether two interfaces are
    equivalent, i.e., whether they define the same entities.
-}
module Curry.Syntax.InterfaceEquivalence (fixInterface, intfEquiv) where

import Data.List (deleteFirstsBy)
import qualified Data.Set as Set

import Curry.Base.Ident
import Curry.Syntax

infix 4 =~=, `eqvSet`

-- |Are two given interfaces equivalent?
intfEquiv :: Interface -> Interface -> Bool
intfEquiv = (=~=)

-- |Type class to express the equivalence of two values
class Equiv a where
  (=~=) :: a -> a -> Bool

instance Equiv a => Equiv (Maybe a) where
  Nothing =~= Nothing = True
  Nothing =~= Just _  = False
  Just _  =~= Nothing = False
  Just x  =~= Just y  = x =~= y

instance Equiv a => Equiv [a] where
  []     =~= []     = True
  (x:xs) =~= (y:ys) = x =~= y && xs =~= ys
  _      =~= _      = False

eqvSet :: Equiv a => [a] -> [a] -> Bool
xs `eqvSet` ys = null (deleteFirstsBy (=~=) xs ys ++ deleteFirstsBy (=~=) ys xs)

instance Equiv Interface where
  Interface m1 is1 ds1 =~= Interface m2 is2 ds2
    = m1 == m2 && is1 `eqvSet` is2 && ds1 `eqvSet` ds2

instance Equiv IImportDecl where
  IImportDecl _ m1 =~= IImportDecl _ m2 = m1 == m2

instance Equiv IDecl where
  IInfixDecl _ fix1 p1 op1 =~= IInfixDecl _ fix2 p2 op2
    = fix1 == fix2 && p1 == p2 && op1 == op2
  HidingDataDecl _ tc1 tvs1 =~= HidingDataDecl _ tc2 tvs2
    = tc1 == tc2 && tvs1 == tvs2
  IDataDecl _ tc1 tvs1 cs1 =~= IDataDecl _ tc2 tvs2 cs2
    = tc1 == tc2 && tvs1 == tvs2 && cs1 =~= cs2
  INewtypeDecl _ tc1 tvs1 nc1 =~= INewtypeDecl _ tc2 tvs2 nc2
    = tc1 == tc2 && tvs1 == tvs2 && nc1 =~= nc2
  ITypeDecl _ tc1 tvs1 ty1 =~= ITypeDecl _ tc2 tvs2 ty2
    = tc1 == tc2 && tvs1 == tvs2 && ty1 == ty2
  IFunctionDecl _ f1 n1 ty1 =~= IFunctionDecl _ f2 n2 ty2
    = f1 == f2 && n1 == n2 && ty1 == ty2
  _ =~= _ = False

instance Equiv ConstrDecl where
  ConstrDecl _ evs1 c1 tys1 =~= ConstrDecl _ evs2 c2 tys2
    = c1 == c2 && evs1 == evs2 && tys1 == tys2
  ConOpDecl _ evs1 ty11 op1 ty12 =~= ConOpDecl _ evs2 ty21 op2 ty22
    = op1 == op2 && evs1 == evs2 && ty11 == ty21 && ty12 == ty22
  RecordDecl _ evs1 c1 fs1 =~= RecordDecl _ evs2 c2 fs2
    = c1 == c2 && evs1 == evs2 && fs1 == fs2
  _ =~= _ = False

instance Equiv NewConstrDecl where
  NewConstrDecl _ evs1 c1 ty1 =~= NewConstrDecl _ evs2 c2 ty2
    = c1 == c2 && evs1 == evs2 && ty1 == ty2

-- If we check for a change in the interface, we do not need to check the
-- interface declarations, but still must disambiguate (nullary) type
-- constructors and type variables in type expressions. This is handled
-- by function 'fixInterface' and the associated type class 'FixInterface'.

-- |Disambiguate nullary type constructors and type variables.
fixInterface :: Interface -> Interface
fixInterface (Interface m is ds) = Interface m is $
  fix (Set.fromList (typeConstructors ds)) ds

class FixInterface a where
  fix :: Set.Set Ident -> a -> a

instance FixInterface a => FixInterface (Maybe a) where
  fix tcs = fmap (fix tcs)

instance FixInterface a => FixInterface [a] where
  fix tcs = map (fix tcs)

instance FixInterface IDecl where
  fix tcs (IDataDecl     p tc tvs cs) = IDataDecl     p tc tvs (fix tcs cs)
  fix tcs (INewtypeDecl  p tc tvs nc) = INewtypeDecl  p tc tvs (fix tcs nc)
  fix tcs (ITypeDecl     p tc tvs ty) = ITypeDecl     p tc tvs (fix tcs ty)
  fix tcs (IFunctionDecl p f  n   ty) = IFunctionDecl p f  n   (fix tcs ty)
  fix _   d                           = d

instance FixInterface ConstrDecl where
  fix tcs (ConstrDecl p evs      c tys) = ConstrDecl p evs c (fix tcs tys)
  fix tcs (ConOpDecl  p evs ty1 op ty2) =
    ConOpDecl p evs (fix tcs ty1) op (fix tcs ty2)
  fix tcs (RecordDecl p evs c fs)       =
    RecordDecl p evs c (map (\(tvs, ty) -> (tvs, fix tcs ty)) fs)

instance FixInterface NewConstrDecl where
  fix tcs (NewConstrDecl p evs c ty) = NewConstrDecl p evs c (fix tcs ty)

instance FixInterface TypeExpr where
  fix tcs (ConstructorType tc tys)
    | not (isQualified tc) && not (isPrimTypeId tc) &&
      tc' `Set.notMember` tcs && null tys
    = VariableType tc'
    | otherwise = ConstructorType tc (fix tcs tys)
    where tc' = unqualify tc
  fix tcs (VariableType  tv)
    | tv `Set.member` tcs = ConstructorType (qualify tv) []
    | otherwise = VariableType tv
  fix tcs (TupleType     tys) = TupleType  (fix tcs tys)
  fix tcs (ListType       ty) = ListType   (fix tcs ty)
  fix tcs (ArrowType ty1 ty2) = ArrowType  (fix tcs ty1) (fix tcs ty2)
  fix tcs (RecordType     fs) = RecordType (map fixField fs)
   where fixField (lbl, ty) = (lbl, fix tcs ty)

typeConstructors :: [IDecl] -> [Ident]
typeConstructors ds = [tc | (QualIdent Nothing tc) <- foldr tyCons [] ds]
  where tyCons (IInfixDecl      _ _ _ _) tcs = tcs
        tyCons (HidingDataDecl   _ tc _) tcs = tc : tcs
        tyCons (IDataDecl      _ tc _ _) tcs = tc : tcs
        tyCons (INewtypeDecl   _ tc _ _) tcs = tc : tcs
        tyCons (ITypeDecl      _ tc _ _) tcs = tc : tcs
        tyCons (IFunctionDecl   _ _ _ _) tcs = tcs

isPrimTypeId :: QualIdent -> Bool
isPrimTypeId tc = tc `elem` [qUnitId, qListId] || isQTupleId tc
