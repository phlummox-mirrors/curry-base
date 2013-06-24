{- |
    Module      :  $Header$
    Description :  Check the equality of two FlatCurry interfaces
    Copyright   :  (c) 2006, Martin Engelke (men@informatik.uni-kiel.de)
                       2011, Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}

module Curry.ExtendedFlat.InterfaceEquality (eqInterface) where

import Data.List (sort)

import Curry.ExtendedFlat.Type

-- |Check whether the interfaces of two FlatCurry programs are equal
eqInterface :: Prog -> Prog -> Bool
eqInterface (Prog mi1 is1 ts1 fs1 os1) (Prog mi2 is2 ts2 fs2 os2)
  =  mi1 == mi2
  && is1 `eqImports`   is2
  && ts1 `eqTypeDecls` ts2
  && fs1 `eqFuncDecls` fs2
  && os1 `eqOpDecls`   os2

eqImports :: [String] -> [String] -> Bool
eqImports is1 is2 = sort is1 == sort is2

eqTypeDecls :: [TypeDecl] -> [TypeDecl] -> Bool
eqTypeDecls ts1 []                               = null ts1
eqTypeDecls ts1 (Type qname vis2 is2 cs2 : ts2') =
  let (mt,ts1') = extract (isDataType qname) ts1
  in  maybe False
            (\ t -> case t of
              (Type _ vis1 is1 cs1) ->
                   vis1 == vis2
                && is1 == is2
                && eqConsDecls cs1 cs2
                && eqTypeDecls ts1' ts2'
              _ -> error "Check.InterfaceCheck.checkTypeDecls: no Type"
            )
            mt
eqTypeDecls ts1 ((TypeSyn qname vis2 is2 texpr2):ts2') =
  let (mt,ts1') = extract (isTypeSyn qname) ts1
  in  maybe False
            (\ t -> case t of
              (TypeSyn _ vis1 is1 texpr1) ->
                   vis1 == vis2
                && is1 == is2
                && texpr1 == texpr2
                && eqTypeDecls ts1' ts2'
              _ -> error "Check.InterfaceCheck.checkTypeDecls: no TypeSyn"
            )
            mt

--
isDataType :: QName -> TypeDecl -> Bool
isDataType qname (Type qname' _ _ _) = qname == qname'
isDataType _     _                   = False

--
eqConsDecls :: [ConsDecl] -> [ConsDecl] -> Bool
eqConsDecls cs1 [] = null cs1
eqConsDecls cs1 ((Cons qname arity2 vis2 texprs2):cs2') =
  let (mc,cs1') = extract (isCons qname) cs1
  in  maybe False
            (\ (Cons _ arity1 vis1 texprs1)
                -> arity1 == arity2
                && vis1 == vis2
                && texprs1 == texprs2
                && eqConsDecls cs1' cs2'
            )
            mc

--
eqFuncDecls :: [FuncDecl] -> [FuncDecl] -> Bool
eqFuncDecls fs1 [] = null fs1
eqFuncDecls fs1 ((Func qname arity2 vis2 texpr2 rule2):fs2') =
  let (mf,fs1') = extract (isFunc qname) fs1
  in  maybe False
            (\ (Func _ arity1 vis1 texpr1 rule1)
                -> arity1 == arity2
                && vis1 == vis2
                && texpr1 == texpr2
                && eqRule rule1 rule2
                && eqFuncDecls fs1' fs2'
            )
            mf

--
eqRule :: Rule -> Rule -> Bool
eqRule (Rule   _ _) (Rule   _ _) = True
eqRule (External _) (External _) = True
eqRule _            _            = False

--
eqOpDecls :: [OpDecl] -> [OpDecl] -> Bool
eqOpDecls os1 [] = null os1
eqOpDecls os1 ((Op qname fix2 prec2):os2') =
  let (mo,os1') = extract (isOp qname) os1
  in  maybe False
            (\ (Op _ fix1 prec1)
                -> prec1 == prec2
                && fix1 == fix2
                && eqOpDecls os1' os2'
            )
            mo

--
isTypeSyn :: QName -> TypeDecl -> Bool
isTypeSyn qname (TypeSyn qname' _ _ _) = qname == qname'
isTypeSyn _     _                      = False

--
isCons :: QName -> ConsDecl -> Bool
isCons qname (Cons qname' _ _ _) = qname == qname'

--
isFunc :: QName -> FuncDecl -> Bool
isFunc qname (Func qname' _ _ _ _) = qname == qname'

--
isOp :: QName -> OpDecl -> Bool
isOp qname (Op qname' _ _) = qname == qname'

--
extract :: (a -> Bool) -> [a] -> (Maybe a, [a])
extract _ []                 = (Nothing, [])
extract p (x:xs) | p x       = (Just x, xs)
                 | otherwise = (res, x:xs') where (res, xs') = extract p xs
{-
-- Alternativ:
extract :: (a -> Bool) -> [a] -> (Maybe a, [a])
extract c xs = maybe (Nothing, xs) (\x -> (Just x, delete x xs)) (find c xs)
-}
