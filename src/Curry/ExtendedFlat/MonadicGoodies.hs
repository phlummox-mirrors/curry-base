{- |
    Module      :  $Header$
    Description :  Monadic transformations of ExtendedFlat programs
    Copyright   :  (c) 2009, Holger Siegel
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}

module Curry.ExtendedFlat.MonadicGoodies
  ( UpdateM, postOrderM, updFuncExpsM, updProgFuncsM, updFuncLetsM
  ) where

import Control.Monad
import Curry.ExtendedFlat.Type

-- |Monadic update of a type's component
type UpdateM m a b = (b -> m b) -> a -> m a

-- |Update an 'Expr' in post-order
postOrderM :: Monad m => UpdateM m Expr Expr
postOrderM f = po where
  po e@(Var _)       = f e
  po e@(Lit _)       = f e
  po (Comb t n es)   = do es' <- mapM po es
                          f (Comb t n es')
  po (Free vs e)     = do e' <- po e
                          f (Free vs e')
  po (Let bs e)      = do bs' <- mapM poBind bs
                          e'  <- po e
                          f (Let bs' e')
  po (Or l r)        = liftM2 Or (po l) (po r) >>= f
  po (Case r t e bs) = do e' <- po e
                          bs' <- mapM poBranch bs
                          f (Case r t e' bs')
  po (Typed e ty)    = do e' <- po e
                          f (Typed e' ty)
  poBind  (v, rhs) = do rhs' <- po rhs
                        return (v, rhs')
  poBranch (Branch p rhs) = do rhs' <- po rhs
                               return (Branch p rhs')

-- |Update all 'Expr's in a function declaration in post-order
updFuncExpsM :: Monad m => UpdateM m FuncDecl Expr
updFuncExpsM f (Func name arity visibility ftype (Rule vs e)) = do
  e' <- postOrderM f e
  return (Func name arity visibility ftype (Rule vs e'))
updFuncExpsM _ func@(Func _ _ _ _ (External _)) = return func

-- |Update all 'FuncDecl's in a program
updProgFuncsM :: Monad m => UpdateM m Prog FuncDecl
updProgFuncsM f (Prog name imps types funcs ops) = do
  funcs' <- mapM f funcs
  return (Prog name imps types funcs' ops)

-- |Update all let-declarations in a function declaration
updFuncLetsM  :: Monad m => ([(VarIndex, Expr)] -> Expr -> m Expr)
              -> FuncDecl -> m FuncDecl
updFuncLetsM = updFuncExpsM . updExprLetsM where
  updExprLetsM f (Let bs e) = f bs e
  updExprLetsM _ e          = return e
