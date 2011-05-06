{- |Free and bound variables

    The compiler needs to compute the sets of free and bound variables for
    various different entities. We will devote three type classes to that
    purpose. The \texttt{QualExpr} class is expected to take into account
    that it is possible to use a qualified name to refer to a function
    defined in the current module and therefore \emph{M.x} and $x$, where
    $M$ is the current module name, should be considered the same name.
    However note that this is correct only after renaming all local
    definitions as \emph{M.x} always denotes an entity defined at the
    top-level.
-}
module Curry.Base.Expr (Expr (..), QualExpr (..), QuantExpr (..)) where

import Curry.Base.Ident

class Expr e where
  fv :: e -> [Ident]

class QualExpr e where
  qfv :: ModuleIdent -> e -> [Ident]

class QuantExpr e where
  bv :: e -> [Ident]

instance Expr e => Expr [e] where
  fv = concatMap fv

instance QualExpr e => QualExpr [e] where
  qfv m = concatMap (qfv m)

instance QuantExpr e => QuantExpr [e] where
  bv = concatMap bv
