{- |
    Module      :  $Header$
    Description :  Utility functions for Curry's abstract syntax
    Copyright   :  (c) 1999-2004 Wolfgang Lux
                       2005 Martin Engelke
                       2011 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module provides some utility functions for working with the
    abstract syntax tree of Curry.
-}
module Curry.Syntax.Utils
  ( isTypeSig, infixOp, isTypeDecl, isValueDecl, isInfixDecl, isClassDecl
  , isInstanceDecl, isRecordDecl, isFunctionDecl, patchModuleId
  , flatLhs, mkInt, fieldLabel, fieldTerm, field2Tuple, opName
  , typeVarsInTypeExpr, typeVarsInContext
  , addSrcRefs, simpleContextToContext
  ) where

import Control.Monad.State
import Data.Generics

import Curry.Base.Ident
import Curry.Base.Position
import Curry.Syntax.Type

import Curry.Files.PathUtils

-- |Replace the generic module name @main@ with the module name derived
-- from the 'FilePath' of the module.
patchModuleId :: FilePath -> Module -> Module
patchModuleId fn m@(Module mid es is ds)
  | mid == mainMIdent = Module (mkMIdent [takeBaseName fn]) es is ds
  | otherwise         = m

-- |Is the declaration an infix declaration?
isInfixDecl :: Decl -> Bool
isInfixDecl (InfixDecl _ _ _ _) = True
isInfixDecl _                   = False

-- |Is the declaration a type declaration?
isTypeDecl :: Decl -> Bool
isTypeDecl (DataDecl    _ _ _ _) = True
isTypeDecl (NewtypeDecl _ _ _ _) = True
isTypeDecl (TypeDecl    _ _ _ _) = True
isTypeDecl _                     = False

-- |Is the declaration a type signature?
isTypeSig :: Decl -> Bool
isTypeSig (TypeSig       _ _ _ _) = True
isTypeSig (ForeignDecl _ _ _ _ _) = True
isTypeSig _                       = False

-- |Is the declaration a value declaration?
isValueDecl :: Decl -> Bool
isValueDecl (FunctionDecl    _ _ _) = True
isValueDecl (ForeignDecl _ _ _ _ _) = True
isValueDecl (ExternalDecl      _ _) = True
isValueDecl (PatternDecl     _ _ _) = True
isValueDecl (FreeDecl          _ _) = True
isValueDecl _ = False

-- |Is the declaration a record declaration?
isRecordDecl :: Decl -> Bool
isRecordDecl (TypeDecl _ _ _ (RecordType _ _)) = True
isRecordDecl _                                 = False

-- |Is the declaration a class declaration?
isClassDecl :: Decl -> Bool
isClassDecl (ClassDecl _ _ _ _ _) = True
isClassDecl _ = False

-- |Is the declaration an instance declaration?
isInstanceDecl :: Decl -> Bool
isInstanceDecl (InstanceDecl _ _ _ _ _ _) = True
isInstanceDecl _ = False

-- |Is the declaraion a
isFunctionDecl :: Decl -> Bool
isFunctionDecl (FunctionDecl _ _ _) = True
isFunctionDecl _ = False

-- |Convert an infix operator into an expression
infixOp :: InfixOp -> Expression
infixOp (InfixOp     op) = Variable op
infixOp (InfixConstr op) = Constructor op

-- |flatten the left-hand-side to the identifier and all constructor terms
flatLhs :: Lhs -> (Ident, [Pattern])
flatLhs lhs = flat lhs []
  where flat (FunLhs    f ts) ts' = (f, ts ++ ts')
        flat (OpLhs t1 op t2) ts' = (op, t1 : t2 : ts')
        flat (ApLhs  lhs' ts) ts' = flat lhs' (ts ++ ts')

-- |Construct an Integer literal
mkInt :: Integer -> Literal
mkInt i = mk (\r -> Int (addPositionIdent (AST r) anonId) i)

-- |Select the label of a field
fieldLabel :: Field a -> Ident
fieldLabel (Field _ l _) = l

-- |Select the term of a field
fieldTerm :: Field a -> a
fieldTerm (Field _ _ t) = t

-- |Select the label and term of a field
field2Tuple :: Field a -> (Ident, a)
field2Tuple (Field _ l t) = (l, t)

-- |Get the operator name of an infix operator
opName :: InfixOp -> QualIdent
opName (InfixOp    op) = op
opName (InfixConstr c) = c

-- |Extract all type variables from the type expression
typeVarsInTypeExpr :: TypeExpr -> [Ident]
typeVarsInTypeExpr (ConstructorType _ ts) = concatMap typeVarsInTypeExpr ts
typeVarsInTypeExpr (SpecialConstructorType _ ts) = concatMap typeVarsInTypeExpr ts
typeVarsInTypeExpr (VariableType t) = [t]
typeVarsInTypeExpr (TupleType ts) = concatMap typeVarsInTypeExpr ts
typeVarsInTypeExpr (ListType t) = typeVarsInTypeExpr t
typeVarsInTypeExpr (ArrowType t1 t2) = typeVarsInTypeExpr t1 ++ typeVarsInTypeExpr t2
typeVarsInTypeExpr (RecordType _ _ ) = error "typeVarsInTypeExpr"

-- |Extract all type variables from a context
typeVarsInContext :: Context -> [Ident]
typeVarsInContext (Context elems) 
  = concatMap (\(ContextElem _qid id0 texp) -> 
               id0 : concatMap typeVarsInTypeExpr texp) elems 

simpleContextToContext :: SContext -> Context
simpleContextToContext (SContext list) 
  = Context $ map (\(qid, id) -> ContextElem qid id []) list

---------------------------
-- add source references
---------------------------

-- |Monad for adding source references
type M a = a -> State Int a

-- |Add 'SrcRef's to a 'Module'
addSrcRefs :: Module -> Module
addSrcRefs x = evalState (addRefs x) 0
  where
  addRefs :: Data a' => M a'
  addRefs = down  `extM` addRefPos
                  `extM` addRefSrc
                  `extM` addRefIdent
                  `extM` addRefListPat
                  `extM` addRefListExp
    where
    down :: Data a' => M a'
    down = gmapM addRefs

    nextRef :: State Int SrcRef
    nextRef = do
      i <- get
      put $! i+1
      return $ srcRef i

    addRefSrc :: M SrcRef
    addRefSrc _ = nextRef

    addRefPos :: M [SrcRef]
    addRefPos _ = (:[]) `liftM` nextRef

    addRefIdent :: M Ident
    addRefIdent ident = flip addRefId ident `liftM` nextRef

    addRefListPat :: M Pattern
    addRefListPat (ListPattern _ ts) = uncurry ListPattern `liftM` addRefList ts
    addRefListPat ct                 = down ct

    addRefListExp :: M Expression
    addRefListExp (List _ ts) = uncurry List `liftM` addRefList ts
    addRefListExp ct          = down ct

    addRefList :: Data a' => [a'] -> State Int ([SrcRef],[a'])
    addRefList ts = do
      i <- nextRef
      let add t = do t' <- addRefs t; j <- nextRef; return (j, t')
      ists <- sequence (map add ts)
      let (is,ts') = unzip ists
      return (i:is,ts')
