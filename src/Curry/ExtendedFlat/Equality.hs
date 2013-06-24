
{- |
  
  This module contains functions for testing whether two flat curry 
  programs are the same except for alpha equivalence (renaming of 
  variables or type variables). 
   
-}

module Curry.ExtendedFlat.Equality (progsEqual) where

import Prelude hiding (concat, zip, zipWith)
import Control.Monad
import Data.Maybe
-- import Base.Utils
import Data.List hiding (concat, zip, zipWith)

import Curry.ExtendedFlat.Type

progsEqual :: Prog -> Prog -> (Bool, String)
progsEqual p1 p2 = case eqProg p1 p2 of
  Nothing -> (False, "structure")
  Just (tvarm, pvarm) -> 
    let tvarm' = nub tvarm
        pvarm' = nub pvarm
    in if bijective tvarm' then 
          if bijective pvarm' 
          then (True, "")
          else (False, "program variables")
       else (False, "type variables")
    
  where
  bijective :: Eq a => [(a, a)] -> Bool
  bijective m = injective m && injective (map (\(x, y) -> (y, x)) m)
  injective :: Eq a => [(a, a)] -> Bool
  injective m = isNothing $ findDouble (map snd m) 
    

type Mapping = ([(TVarIndex, TVarIndex)], [(VarIndex, VarIndex)])

eqProg :: Prog -> Prog -> Maybe Mapping
eqProg (Prog s1 ss1 tds1 fds1 opds1)
       (Prog s2 ss2 tds2 fds2 opds2)
  | s1 == s2 && ss1 == ss2 = do
    m1 <- zipWith'' eqTypeDecl tds1 tds2
    m2 <- zipWith'' eqFuncDecl fds1 fds2
    m3 <- zipWith'' eqOpDecl opds1 opds2
    return (m1 `concat` m2 `concat` m3) 
  | otherwise = Nothing


  
eqTypeDecl :: TypeDecl -> TypeDecl -> Maybe Mapping
eqTypeDecl (Type n1 v1 is1 csd1) (Type n2 v2 is2 csd2)
  | n1 == n2 && v1 == v2 = do
    m1 <- zipWith'' tVars is1 is2 
    m2 <- zipWith'' eqConsDecl csd1 csd2
    return (m1 `concat` m2)
  | otherwise = Nothing
eqTypeDecl (TypeSyn q1 v1 is1 te1) (TypeSyn q2 v2 is2 te2) 
  | q1 == q2 && v1 == v2 = do
    m1 <- zipWith'' tVars is1 is2
    m2 <- eqTypeExpr te1 te2
    return (m1 `concat` m2)
  | otherwise = Nothing
eqTypeDecl _ _ = Nothing

eqFuncDecl :: FuncDecl -> FuncDecl -> Maybe Mapping 
eqFuncDecl (Func q1 a1 v1 te1 r1) (Func q2 a2 v2 te2 r2) 
  | q1 == q2 && a1 == a2 && v1 == v2 = do
    m1 <- eqTypeExpr te1 te2
    m2 <- eqRule r1 r2
    return (m1 `concat` m2)
  | otherwise = Nothing 
    
eqConsDecl :: ConsDecl -> ConsDecl -> Maybe Mapping
eqConsDecl (Cons q1 a1 v1 tes1) (Cons q2 a2 v2 tes2) 
  | q1 == q2 && a1 == a2 && v1 == v2 = 
    zipWith'' eqTypeExpr tes1 tes2
  | otherwise = Nothing

eqOpDecl :: OpDecl -> OpDecl -> Maybe Mapping
eqOpDecl (Op q1 f1 p1) (Op q2 f2 p2)
  | q1 == q2 && f1 == f2 && p1 == p2 = 
    emptyMapping
  | otherwise = Nothing

eqTypeExpr :: TypeExpr -> TypeExpr -> Maybe Mapping
eqTypeExpr (TVar i1) (TVar i2) = tVars i1 i2
eqTypeExpr (FuncType te11 te12) (FuncType te21 te22) = do
  m1 <- eqTypeExpr te11 te21
  m2 <- eqTypeExpr te12 te22
  return (m1 `concat` m2)
eqTypeExpr (TCons c1 es1) (TCons c2 es2)
  | c1 == c2 = zipWith'' eqTypeExpr es1 es2
  | otherwise = Nothing
eqTypeExpr _ _ = Nothing
  
eqRule :: Rule -> Rule -> Maybe Mapping
eqRule (External s1) (External s2) | s1 == s2 = emptyMapping
                                   | otherwise = Nothing
eqRule (Rule vs1 e1) (Rule vs2 e2) = do 
  m1 <- zipWith'' pVars vs1 vs2
  m2 <- eqExpr e1 e2
  return (m1 `concat` m2)
eqRule _ _ = Nothing 

eqExpr :: Expr -> Expr -> Maybe Mapping
eqExpr (Var v1) (Var v2) = pVars v1 v2
eqExpr (Lit l1) (Lit l2) = if l1 == l2 then emptyMapping else Nothing
eqExpr (Comb c1 q1 es1) (Comb c2 q2 es2) 
  | c1 == c2 && q1 == q2 = zipWith'' eqExpr es1 es2
  | otherwise = Nothing
eqExpr (Free vs1 e1) (Free vs2 e2) = do
  m1 <- zipWith'' pVars vs1 vs2
  m2 <- eqExpr e1 e2
  return (m1 `concat` m2)
eqExpr (Let ls1 e1) (Let ls2 e2) = do
  let vs1 = map fst ls1
      vs2 = map fst ls2
      es1 = map snd ls1
      es2 = map snd ls2
  m1 <- zipWith'' pVars vs1 vs2
  m2 <- zipWith'' eqExpr es1 es2
  m3 <- eqExpr e1 e2
  return (m1 `concat` m2 `concat` m3)
eqExpr (Or e11 e12) (Or e21 e22) = do
  m1 <- eqExpr e11 e21
  m2 <- eqExpr e12 e22
  return (m1 `concat` m2)
eqExpr (Case s1 ct1 e1 bes1) (Case s2 ct2 e2 bes2) 
  | s1 == s2 && ct1 == ct2 = do
    m1 <- eqExpr e1 e2
    m2 <- zipWith'' eqBExpr bes1 bes2
    return (m1 `concat` m2)
  | otherwise = Nothing
eqExpr (Typed e1 te1) (Typed e2 te2) = do
  m1 <- eqExpr e1 e2
  m2 <- eqTypeExpr te1 te2
  return (m1 `concat` m2)
eqExpr _ _ = Nothing


eqBExpr :: BranchExpr -> BranchExpr -> Maybe Mapping
eqBExpr (Branch ps1 e1) (Branch ps2 e2) = do
  m1 <- eqPattern ps1 ps2
  m2 <- eqExpr e1 e2
  return (m1 `concat` m2)
  
eqPattern :: Pattern -> Pattern -> Maybe Mapping
eqPattern (LPattern l1) (LPattern l2) = 
  if l1 == l2 then emptyMapping else Nothing
eqPattern (Pattern q1 vs1) (Pattern q2 vs2) 
  | q1 == q2 = zipWith'' pVars vs1 vs2
  | otherwise = Nothing 
eqPattern _ _ = Nothing

-- -------------------------------------------------------------------------
-- helper functions
-- -------------------------------------------------------------------------

zipWith'' :: (a -> b -> Maybe Mapping) -> [a] -> [b] -> Maybe Mapping 
zipWith'' f xs ys = zipWith' f xs ys concat (fromJust emptyMapping) 

zipWith' :: (a -> b -> Maybe c) -> [a] -> [b] -> (c -> c -> c) -> c -> Maybe c
zipWith' f xs ys conc ne = liftM (foldl conc ne) $ zipWith f xs ys 

zipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWith _ [] (_:_) = Nothing
zipWith _ (_:_) [] = Nothing
zipWith f (x:xs) (y:ys) = do
  el1 <- f x y
  rest <- zipWith f xs ys
  return (el1 : rest)
zipWith _ [] [] = return []

{-
zip :: [a] -> [b] -> Maybe ([(a, b)])
zip = zipWith (\x y -> Just (x, y))

zip' :: [a] -> [b] -> ((a, b) -> (a, b) -> (a, b)) -> Maybe (a, b)
zip' xs ys conc = liftM (foldr1 conc) $ zip xs ys
-}

concat :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concat (xs1, ys1) (xs2, ys2) = (xs1 ++ xs2, ys1 ++ ys2) 


emptyMapping :: Maybe Mapping
emptyMapping = Just ([], [])

tVars :: TVarIndex -> TVarIndex -> Maybe Mapping
tVars i1 i2 = Just ([(i1, i2)], [])

pVars :: VarIndex -> VarIndex -> Maybe Mapping
pVars i1 i2 = Just ([], [(i1, i2)])


findDouble :: Eq a => [a] -> Maybe a
findDouble []   = Nothing
findDouble (x : xs)
  | x `elem` xs = Just x
  | otherwise   = findDouble xs
