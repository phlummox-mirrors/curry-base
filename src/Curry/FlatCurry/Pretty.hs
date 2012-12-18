{- |
    Module      :  $Header$
    Description :  A pretty printer for FlatCurry
    Copyright   :  (c) 2012 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module implements a pretty printer for FlatCurry modules.
-}
module Curry.FlatCurry.Pretty
  ( ppProg, ppHeader, ppExports, ppImport, ppTypeDecl, ppTypeExpr
  , ppFuncDecl, ppExpr, ppLiteral, ppOpDecl
  ) where

import Data.Char (ord)
import Text.PrettyPrint

import Curry.FlatCurry.Type

-- |pretty-print a FlatCurry module
ppProg :: Prog -> Doc
ppProg (Prog m is ts fs os) = ppHeader m ts fs
  $$ vcat (map ppImport   is)
  $$ vcat (map ppOpDecl   os)
  $$ vcat (map ppTypeDecl ts)
  $$ vcat (map ppFuncDecl fs)

-- |pretty-print the module header
ppHeader :: String -> [TypeDecl] -> [FuncDecl] -> Doc
ppHeader m ts fs = sep
  [text "module" <+> text m, ppExports ts fs, text "where"]

-- |pretty-print the export list
ppExports :: [TypeDecl] -> [FuncDecl] -> Doc
ppExports ts fs = parens $ list (map ppTypeExport ts ++ ppFuncExports fs)

-- |pretty-print a type export
ppTypeExport :: TypeDecl -> Doc
ppTypeExport (Type    qn vis _ cs)
  | vis == Private      = empty
  | all isPublicCons cs = ppPrefixOp qn <+> text "(..)"
  | otherwise           = ppPrefixOp qn <+> parens (list (ppConsExports cs))
    where isPublicCons (Cons _ _ v _) = v == Public
ppTypeExport (TypeSyn qn vis _ _ )
  | vis == Private = empty
  | otherwise      = ppPrefixOp qn

-- |pretty-print the export list of constructors
ppConsExports :: [ConsDecl] -> [Doc]
ppConsExports cs = [ ppPrefixOp qn | Cons qn _ Public _ <- cs]

-- |pretty-print the export list of functions
ppFuncExports :: [FuncDecl] -> [Doc]
ppFuncExports fs = [ ppPrefixOp qn | Func qn _ Public _ _ <- fs]

-- |pretty-print an import statement
ppImport :: String -> Doc
ppImport m = text "import" <+> text m

-- |pretty-print a type declaration
ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl (Type    qn _ ixs cs) = text "data" <+> ppQName qn
  <+> hcat (map ppTVarIndex ixs) $+$ ppConsDecls cs
ppTypeDecl (TypeSyn qn _ ixs ty) = text "type" <+> ppQName qn
  <+> hcat (map ppTVarIndex ixs) <+> equals <+> ppTypeExpr 0 ty

-- |pretty-print the constructor declarations
ppConsDecls :: [ConsDecl] -> Doc
ppConsDecls cs = indent $ vcat $
  zipWith (<+>) (equals : repeat vbar) (map ppConsDecl cs)

-- |pretty print a single constructor
ppConsDecl :: ConsDecl -> Doc
ppConsDecl (Cons qn _ _ tys) = fsep $ ppPrefixOp qn : map (ppTypeExpr 2) tys

-- |pretty-print a type expression
ppTypeExpr :: Int -> TypeExpr -> Doc
ppTypeExpr _ (TVar         idx) = ppTVarIndex idx
ppTypeExpr p (FuncType ty1 ty2) = parenIf (p > 0) $ fsep
  [ppTypeExpr 1 ty1, rarrow, ppTypeExpr 0 ty2]
ppTypeExpr p (TCons     qn tys) = parenIf (p > 1 && not (null tys)) $ fsep
  (ppPrefixOp qn : map (ppTypeExpr 2) tys)

-- |pretty-print a type variable
ppTVarIndex :: TVarIndex -> Doc
ppTVarIndex i = text $ vars !! i
  where vars = [ if n == 0 then [c] else c : show n
               | n <- [0 :: Int ..], c <- ['a' .. 'z']
               ]

-- |pretty-print a function declaration
ppFuncDecl :: FuncDecl -> Doc
ppFuncDecl (Func qn _ _ ty rule)
  = hsep [ppPrefixOp qn, text "::", ppTypeExpr 0 ty]
    $+$ ppPrefixOp qn <+> ppRule rule

-- |pretty-print a function rule
ppRule :: Rule -> Doc
ppRule (Rule  vs e) = fsep (map ppVarIndex vs) <+> equals
                      <+> indent (ppExpr 0 e)
ppRule (External _) = text "external"

-- |pretty-print an expression
ppExpr :: Int -> Expr -> Doc
ppExpr _ (Var        v) = ppVarIndex v
ppExpr _ (Lit        l) = ppLiteral l
ppExpr p (Comb _ qn es) = parenIf (p > 0)
                        $ ppPrefixOp qn <+> fsep (map (ppExpr 1) es)
ppExpr p (Free    vs e) = parenIf (p > 0) $ sep
                          [ letSym <+> list (map ppVarIndex vs) <+> free
                          , inSym <+> ppExpr 0 e
                          ]
ppExpr p (Let     ds e) = parenIf (p > 0) $ sep
                          [letSym <+> ppDecls ds, inSym <+> ppExpr 0 e]
ppExpr p (Or     e1 e2) = parenIf (p > 0)
                        $ ppExpr 1 e1 <+> text "?" <+> ppExpr 1 e2
ppExpr p (Case ct e bs) = parenIf (p > 0)
                        $ ppCaseType ct <+> ppExpr 0 e <+> text "of"
                          $$ indent (vcat (map ppBranch bs))
ppExpr p (Typed   e ty) = parenIf (p > 0)
                        $ ppExpr 0 e <+> text "::" <+> ppTypeExpr 0 ty

-- |pretty-print a list of declarations
ppDecls :: [(VarIndex, Expr)] -> Doc
ppDecls = vcat . map ppDecl

-- |pretty-print a single declaration
ppDecl :: (VarIndex, Expr) -> Doc
ppDecl (v, e) = ppVarIndex v <+> equals <+> ppExpr 0 e

-- |pretty-print the (f)case keyword
ppCaseType :: CaseType -> Doc
ppCaseType Rigid = text "case"
ppCaseType Flex  = text "fcase"

-- |pretty-print a case branch
ppBranch :: BranchExpr -> Doc
ppBranch (Branch p e) = ppPattern p <+> rarrow <+> ppExpr 0 e

-- |pretty-print a pattern
ppPattern :: Pattern -> Doc
ppPattern (Pattern  qn vs) = ppPrefixOp qn <+> fsep (map ppVarIndex vs)
ppPattern (LPattern     l) = ppLiteral l

-- |pretty-print a literal
ppLiteral :: Literal -> Doc
ppLiteral (Intc   i) = integer i
ppLiteral (Floatc f) = double  f
ppLiteral (Charc  c) = text (showEscape c)

showEscape :: Char -> String
showEscape c
  | o <   10  = "'\\00" ++ show o ++ "'"
  | o <   32  = "'\\0"  ++ show o ++ "'"
  | o == 127  = "'\\127'"
  | otherwise = show c
  where o = ord c

-- |pretty-print a operator fixity declaration
ppOpDecl :: OpDecl -> Doc
ppOpDecl (Op qn fix n) = ppFixity fix <+> int n <+> ppInfixOp qn

-- |pretty-print the associativity keyword
ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

-- |pretty-print a variable
ppVarIndex :: VarIndex -> Doc
ppVarIndex i = text $ 'v' : show i

-- Names

-- |pretty-print a qualified name
ppQName :: QName -> Doc
ppQName (m, i) = text $ concat [m, ".", i]

-- |pretty-print a name in prefix manner
ppPrefixOp :: QName -> Doc
ppPrefixOp qn = parenIf (isInfixOp qn) (ppQName qn)

-- |pretty-print a name in infix manner
ppInfixOp :: QName -> Doc
ppInfixOp qn = backqouteIf (not (isInfixOp qn)) (ppQName qn)

-- |Is a 'QName' an infix operator?
isInfixOp :: QName -> Bool
isInfixOp (_, i) = all (`elem` "~!@#$%^&*+-=<>:?./|\\") i

-- Helper

letSym :: Doc
letSym = text "let"

inSym :: Doc
inSym = text "in"

free :: Doc
free = text "free"

indent :: Doc -> Doc
indent = nest 2

list :: [Doc] -> Doc
list = fsep . punctuate comma

parenIf :: Bool -> Doc -> Doc
parenIf b doc = if b then parens doc else doc

backqouteIf :: Bool -> Doc -> Doc
backqouteIf b doc = if b then backQuote <> doc <> backQuote else doc

vbar :: Doc
vbar = text "|"

rarrow :: Doc
rarrow = text "->"

backQuote :: Doc
backQuote = char '`'
