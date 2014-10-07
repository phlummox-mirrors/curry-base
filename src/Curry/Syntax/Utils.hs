{- |
    Module      :  $Header$
    Description :  Utility functions for Curry's abstract syntax
    Copyright   :  (c) 1999 - 2004 Wolfgang Lux
                       2005        Martin Engelke
                       2011 - 2014 Björn Peemöller
                       2013 Matthias Böhm
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module provides some utility functions for working with the
    abstract syntax tree of Curry.
-}

module Curry.Syntax.Utils
  ( isTypeSig, infixOp, isTypeDecl, isValueDecl, isInfixDecl, isClassDecl
  , isInstanceDecl, isRecordDecl, isFunctionDecl, isPatternDecl, patchModuleId
  , flatLhs, mkInt, mkFloat, fieldLabel, fieldTerm, field2Tuple, opName
  , hasLanguageExtension, knownExtensions
  , isExternalDecl
  , typeVarsInTypeExpr, typeVarsInContext, typeVarsInSContext
  , addSrcRefs, simpleContextToContext
  , isDataDecl, isNewtypeDecl, arrowArityTyExpr
  , specialConsToTyExpr, specialTyConToQualIdent, toTypeConstructor
  , qualifyTypeExpr, unqualifyTypeExpr
  ) where

import Control.Monad.State
import Data.Generics
import Data.Maybe

import Curry.Base.Ident
import Curry.Base.Position
import Curry.Files.Filenames (takeBaseName)
import Curry.Syntax.Extension
import Curry.Syntax.Type

-- |Check whether a 'Module' has a specific 'KnownExtension' enabled by a pragma
hasLanguageExtension :: Module -> KnownExtension -> Bool
hasLanguageExtension mdl ext = ext `elem` knownExtensions mdl

-- |Extract all known extensions from a 'Module'
knownExtensions :: Module -> [KnownExtension]
knownExtensions (Module ps _ _ _ _) =
  [ e | LanguagePragma _ exts <- ps, KnownExtension _ e <- exts]

-- |Replace the generic module name @main@ with the module name derived
-- from the 'FilePath' of the module.
patchModuleId :: FilePath -> Module -> Module
patchModuleId fn m@(Module ps mid es is ds)
  | mid == mainMIdent = Module ps (mkMIdent [takeBaseName fn]) es is ds
  | otherwise         = m

-- |Is the declaration an infix declaration?
isInfixDecl :: Decl -> Bool
isInfixDecl (InfixDecl _ _ _ _) = True
isInfixDecl _                   = False

-- |Is the declaration a type declaration?
isTypeDecl :: Decl -> Bool
isTypeDecl (DataDecl    _ _ _ _ _) = True
isTypeDecl (NewtypeDecl _ _ _ _ _) = True
isTypeDecl (TypeDecl      _ _ _ _) = True
isTypeDecl _                       = False

-- |Is the declaration a type signature?
isTypeSig :: Decl -> Bool
isTypeSig (TypeSig     _ _ _ _ _) = True
isTypeSig (ForeignDecl _ _ _ _ _) = True
isTypeSig _                       = False

-- |Is the declaration a value declaration?
isValueDecl :: Decl -> Bool
isValueDecl (FunctionDecl _ _ _ _ _) = True
isValueDecl (ForeignDecl  _ _ _ _ _) = True
isValueDecl (ExternalDecl       _ _) = True
isValueDecl (PatternDecl  _ _ _ _ _) = True
isValueDecl (FreeDecl           _ _) = True
isValueDecl _                       = False

-- |Is the declaration a record declaration?
isRecordDecl :: Decl -> Bool
isRecordDecl (TypeDecl _ _ _ (RecordType _ _)) = True
isRecordDecl _                                 = False

-- |Is the declaration an external declaration?
isExternalDecl :: Decl -> Bool
isExternalDecl (ForeignDecl _ _ _ _ _) = True
isExternalDecl (ExternalDecl      _ _) = True
isExternalDecl _                       = False

-- |Is the declaration a class declaration?
isClassDecl :: Decl -> Bool
isClassDecl (ClassDecl _ _ _ _ _) = True
isClassDecl _ = False

-- |Is the declaration an instance declaration?
isInstanceDecl :: Decl -> Bool
isInstanceDecl (InstanceDecl _ _ _ _ _ _) = True
isInstanceDecl _ = False

-- |Is the declaration a function declaration?
isFunctionDecl :: Decl -> Bool
isFunctionDecl (FunctionDecl _ _ _ _ _) = True
isFunctionDecl _ = False

-- |Is the declaration a pattern declaration?
isPatternDecl :: Decl -> Bool
isPatternDecl (PatternDecl _ _ _ _ _) = True
isPatternDecl _ = False

-- |Is the declaration a data declaration?
isDataDecl :: Decl -> Bool
isDataDecl (DataDecl _ _ _ _ _) = True
isDataDecl _ = False

-- |Is the declaration a newtype declaration?
isNewtypeDecl :: Decl -> Bool
isNewtypeDecl (NewtypeDecl _ _ _ _ _) = True
isNewtypeDecl _ = False 

-- |Convert an infix operator into an expression, preserving the type annotation!
infixOp :: InfixOp -> Expression
infixOp (InfixOp cty op) = Variable cty op
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

-- |Construct a Float literal
mkFloat :: Double -> Literal
mkFloat f = mk (\r -> Float (addPositionIdent (AST r) anonId) f)

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
opName (InfixOp  _ op) = op
opName (InfixConstr c) = c

-- |Extract all type variables from the type expression
typeVarsInTypeExpr :: TypeExpr -> [Ident]
typeVarsInTypeExpr (ConstructorType _ ts) = concatMap typeVarsInTypeExpr ts
typeVarsInTypeExpr (SpecialConstructorType _ ts) = concatMap typeVarsInTypeExpr ts
typeVarsInTypeExpr (VariableType t) = [t]
typeVarsInTypeExpr (TupleType ts) = concatMap typeVarsInTypeExpr ts
typeVarsInTypeExpr (ListType t) = typeVarsInTypeExpr t
typeVarsInTypeExpr (ArrowType t1 t2) = typeVarsInTypeExpr t1 ++ typeVarsInTypeExpr t2
typeVarsInTypeExpr (RecordType fs mt) = 
  concatMap (typeVarsInTypeExpr . snd) fs 
  ++ concatMap typeVarsInTypeExpr (maybeToList mt) 

-- |Extract all type variables from a context
typeVarsInContext :: Context -> [Ident]
typeVarsInContext (Context elems) 
  = concatMap (\(ContextElem _qid id0 texp) -> 
               id0 : concatMap typeVarsInTypeExpr texp) elems 

simpleContextToContext :: SContext -> Context
simpleContextToContext (SContext list) 
  = Context $ map (\(qid, id0) -> ContextElem qid id0 []) list
  
typeVarsInSContext :: SContext -> [Ident]
typeVarsInSContext (SContext cx) = map snd cx

-- |calculates the arity of a given type signature
arrowArityTyExpr :: TypeExpr -> Int
arrowArityTyExpr (ArrowType _ ty) = 1 + arrowArityTyExpr ty
arrowArityTyExpr (SpecialConstructorType ArrowTC [_, ty]) = 1 + arrowArityTyExpr ty
arrowArityTyExpr _                = 0

-- |transforms a "SpecialConstructorType" into the other data constructors
-- of "TypeExpr". Throws an error if kinds are incorrect. 
specialConsToTyExpr :: TypeExpr -> TypeExpr
specialConsToTyExpr (SpecialConstructorType (QualTC qid) tys) 
  = ConstructorType qid tys
specialConsToTyExpr (SpecialConstructorType UnitTC []) = TupleType []
specialConsToTyExpr (SpecialConstructorType (TupleTC n) tys) 
  | n == length tys = TupleType tys 
specialConsToTyExpr (SpecialConstructorType ListTC [ty]) = ListType ty
specialConsToTyExpr (SpecialConstructorType ArrowTC (t1:t2:[])) = ArrowType t1 t2 
specialConsToTyExpr (SpecialConstructorType _ _) = error "specialConsToTyExpr"
specialConsToTyExpr t = t 

-- |returns a qualified identifiers for the four special type constructors
specialTyConToQualIdent :: TypeConstructor -> QualIdent
specialTyConToQualIdent UnitTC = qUnitIdP
specialTyConToQualIdent (TupleTC n) = qTupleIdP n
specialTyConToQualIdent ListTC = qListIdP
specialTyConToQualIdent ArrowTC = qArrowIdP
specialTyConToQualIdent (QualTC tc) = tc

-- |converts a given identifier to a type constructor, considering special
-- syntax constructors
toTypeConstructor :: QualIdent -> TypeConstructor
toTypeConstructor ty
  | ty == qArrowId || ty == qArrowIdP = ArrowTC
  | ty == qListId  || ty == qListIdP  = ListTC
  | isQTupleId ty                     = TupleTC $ qTupleArity ty
  | ty == qUnitId  || ty == qUnitIdP  = UnitTC
  | otherwise                         = QualTC ty


-- |qualifies a given type expression
qualifyTypeExpr :: ModuleIdent -> TypeExpr -> TypeExpr
qualifyTypeExpr m (ConstructorType qid tys) = 
  ConstructorType (qualQualify m qid) (map (qualifyTypeExpr m) tys)
qualifyTypeExpr _m v@(VariableType _) = v
qualifyTypeExpr m (TupleType tys) = TupleType (map (qualifyTypeExpr m) tys)
qualifyTypeExpr m (ListType ty) = ListType (qualifyTypeExpr m ty)
qualifyTypeExpr m (ArrowType ty1 ty2) = 
  ArrowType (qualifyTypeExpr m ty1) (qualifyTypeExpr m ty2)
qualifyTypeExpr m (RecordType rs mty) = 
  RecordType (map (\(ids, ty) -> (ids, qualifyTypeExpr m ty)) rs)
             (fmap (qualifyTypeExpr m) mty)   
qualifyTypeExpr m (SpecialConstructorType (QualTC qid) tys) =  
  SpecialConstructorType (QualTC $ qualQualify m qid)
    (map (qualifyTypeExpr m) tys)
qualifyTypeExpr m (SpecialConstructorType c tys) = 
  SpecialConstructorType c (map (qualifyTypeExpr m) tys)

-- |unqualify a given type expression
unqualifyTypeExpr :: ModuleIdent -> TypeExpr -> TypeExpr
unqualifyTypeExpr m (ConstructorType qid tys) = 
  ConstructorType (qualUnqualify m qid) (map (unqualifyTypeExpr m) tys)
unqualifyTypeExpr _m v@(VariableType _) = v
unqualifyTypeExpr m (TupleType tys) = TupleType (map (unqualifyTypeExpr m) tys)
unqualifyTypeExpr m (ListType ty) = ListType (unqualifyTypeExpr m ty)
unqualifyTypeExpr m (ArrowType ty1 ty2) = 
  ArrowType (unqualifyTypeExpr m ty1) (unqualifyTypeExpr m ty2)
unqualifyTypeExpr m (RecordType rs mty) = 
  RecordType (map (\(ids, ty) -> (ids, unqualifyTypeExpr m ty)) rs)
             (fmap (unqualifyTypeExpr m) mty)
unqualifyTypeExpr m (SpecialConstructorType (QualTC qid) tys) = 
  SpecialConstructorType (QualTC $ qualUnqualify m qid)
    (map (unqualifyTypeExpr m) tys)
unqualifyTypeExpr m (SpecialConstructorType c tys) = 
  SpecialConstructorType c (map (unqualifyTypeExpr m) tys)

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
