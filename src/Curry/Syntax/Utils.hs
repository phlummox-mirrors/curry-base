module Curry.Syntax.Utils
  ( isEvalAnnot, isTypeSig, infixOp, isTypeDecl, isValueDecl, isInfixDecl
  , isRecordDecl, patchModuleId
  , flatLhs, mk', mk, mkInt, fieldLabel, fieldTerm, field2Tuple, opName
  , addSrcRefs
  ) where

import Control.Monad.State
import Data.Generics

import Curry.Base.Ident
import Curry.Base.Position
import Curry.Syntax.Type

import Curry.Files.PathUtils

-- A module which doesn't contain a \texttt{module ... where} declaration
-- obtains its filename as module identifier (unlike the definition in
-- Haskell and original MCC where a module obtains \texttt{main}).

patchModuleId :: FilePath -> Module -> Module
patchModuleId fn m@(Module mid es is ds)
  | mid == mainMIdent
    = Module (mkMIdent [takeBaseName fn]) es is ds
  | otherwise
    = m

isInfixDecl :: Decl -> Bool
isInfixDecl (InfixDecl _ _ _ _) = True
isInfixDecl _                   = False

isTypeDecl :: Decl -> Bool
isTypeDecl (DataDecl    _ _ _ _) = True
isTypeDecl (NewtypeDecl _ _ _ _) = True
isTypeDecl (TypeDecl    _ _ _ _) = True
isTypeDecl _                     = False

isTypeSig :: Decl -> Bool
isTypeSig (TypeSig      _ _ _    ) = True
isTypeSig (ExternalDecl _ _ _ _ _) = True
isTypeSig _                        = False

isEvalAnnot :: Decl -> Bool
isEvalAnnot (EvalAnnot _ _ _) = True
isEvalAnnot _                 = False

isValueDecl :: Decl -> Bool
isValueDecl (FunctionDecl     _ _ _    ) = True
isValueDecl (ExternalDecl     _ _ _ _ _) = True
isValueDecl (FlatExternalDecl _ _      ) = True
isValueDecl (PatternDecl      _ _ _    ) = True
isValueDecl (ExtraVariables    _ _     ) = True
isValueDecl _ = False

isRecordDecl :: Decl -> Bool
isRecordDecl (TypeDecl _ _ _ (RecordType _ _)) = True
isRecordDecl _                                 = False

-- |Convert an infix operator into an expression
infixOp :: InfixOp -> Expression
infixOp (InfixOp     op) = Variable op
infixOp (InfixConstr op) = Constructor op

flatLhs :: Lhs -> (Ident,[ConstrTerm])
flatLhs lhs = flat lhs []
  where flat (FunLhs f ts)    ts' = (f, ts ++ ts')
        flat (OpLhs t1 op t2) ts' = (op, t1 : t2 : ts')
        flat (ApLhs lhs' ts)  ts' = flat lhs' (ts ++ ts')

mk' :: ([SrcRef] -> a) -> a
mk' = ($ [])

mk :: (SrcRef -> a) -> a
mk = ($ noRef)

mkInt :: Integer -> Literal
mkInt i = mk (\r -> Int (addPositionIdent (AST  r) anonId) i)

fieldLabel :: Field a -> Ident
fieldLabel (Field _ l _) = l

fieldTerm :: Field a -> a
fieldTerm (Field _ _ t) = t

field2Tuple :: Field a -> (Ident,a)
field2Tuple (Field _ l t) = (l, t)

opName :: InfixOp -> QualIdent
opName (InfixOp    op) = op
opName (InfixConstr c) = c

---------------------------
-- add source references
---------------------------

type M a = a -> State Int a

addSrcRefs :: Module -> Module
addSrcRefs x = evalState (addRef' x) 0
  where
  addRef' :: Data a' => M a'
  addRef' = down `extM` addRefPos
                  `extM` addRefSrc
                  `extM` addRefIdent
                  `extM` addRefListPat
                  `extM` addRefListExp
    where
    down :: Data a' => M a'
    down = gmapM addRef'

    addRefPos :: M [SrcRef]
    addRefPos _ = liftM (:[]) nextRef

    addRefSrc :: M SrcRef
    addRefSrc _ = nextRef

    addRefIdent :: M Ident
    addRefIdent ident = liftM (flip addRefId ident) nextRef

    addRefListPat :: M ConstrTerm
    addRefListPat (ListPattern _ ts) = do
      liftM (uncurry ListPattern) (addRefList ts)
    addRefListPat ct = gmapM addRef' ct

    addRefListExp :: M Expression
    addRefListExp (List _ ts) = do
      liftM (uncurry List) (addRefList ts)
    addRefListExp ct = gmapM addRef' ct

    addRefList :: Data a' => [a'] -> State Int ([SrcRef],[a'])
    addRefList ts = do
      i <- nextRef
      let add t = do t' <- addRef' t;j <- nextRef; return (j,t')
      ists <- sequence (map add ts)
      let (is,ts') = unzip ists
      return (i:is,ts')

    nextRef :: State Int SrcRef
    nextRef = do
      i <- get
      put $! i+1
      return (SrcRef [i])
