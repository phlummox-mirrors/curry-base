{- |
    Module      :  $Header$
    Description :  Abstract syntax for Curry
    Copyright   :  (c) 1999-2004 Wolfgang Lux
                       2005 Martin Engelke
                       2011 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  non-portable (DeriveDataTypeable)

    This module provides the necessary data structures to maintain the
    parsed representation of a Curry program.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Curry.Syntax.Type
  ( -- * Module header
    Module (..), ExportSpec (..), Export (..)
  , ImportDecl (..), ImportSpec (..), Import (..), Qualified
    -- * Interface
  , Interface (..), IImportDecl (..), IDecl (..)
    -- * Declarations
  , Decl (..), Infix (..), ConstrDecl (..), NewConstrDecl (..)
  , EvalAnnotation (..), CallConv (..), TypeExpr (..)
  , Equation (..), Lhs (..), Rhs (..), CondExpr (..)
  , Literal (..), ConstrTerm (..), Expression (..), InfixOp (..)
  , Statement (..), Alt (..), Field (..)
  ) where

import Data.Generics (Data(..), Typeable(..))

import Curry.Base.Ident
import Curry.Base.Position

-- ---------------------------------------------------------------------------
-- Modules
-- ---------------------------------------------------------------------------

data Module = Module ModuleIdent (Maybe ExportSpec) [ImportDecl] [Decl]
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Export specification
-- ---------------------------------------------------------------------------

data ExportSpec = Exporting Position [Export]
    deriving (Eq, Read, Show, Data, Typeable)

data Export
  = Export         QualIdent         -- f/T
  | ExportTypeWith QualIdent [Ident] -- T (C1,...,Cn)
  | ExportTypeAll  QualIdent         -- T (..)
  | ExportModule   ModuleIdent       -- module M
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Import declaration and specification
-- ---------------------------------------------------------------------------

data ImportDecl = ImportDecl Position ModuleIdent Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)
    deriving (Eq, Read, Show, Data, Typeable)

type Qualified = Bool

data ImportSpec
  = Importing Position [Import]
  | Hiding Position [Import]
    deriving (Eq, Read, Show, Data, Typeable)

data Import
  = Import         Ident            -- f/T
  | ImportTypeWith Ident [Ident]    -- T (C1,...,Cn)
  | ImportTypeAll  Ident            -- T (..)
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Module interfaces
-- ---------------------------------------------------------------------------

-- | Module interfaces
--
-- Interface declarations are restricted to type declarations and signatures.
-- Note that an interface function declaration additionaly contains the
-- function arity (= number of parameters) in order to generate
-- correct FlatCurry function applications.
data Interface = Interface ModuleIdent [IImportDecl] [IDecl]
    deriving (Eq, Read, Show, Data, Typeable)

data IImportDecl = IImportDecl Position ModuleIdent
    deriving (Eq, Read, Show, Data, Typeable)

data IDecl
  = IInfixDecl     Position Infix Integer QualIdent
  | HidingDataDecl Position Ident [Ident]
  | IDataDecl      Position QualIdent [Ident] [Maybe ConstrDecl]
  | INewtypeDecl   Position QualIdent [Ident] NewConstrDecl
  | ITypeDecl      Position QualIdent [Ident] TypeExpr
  | IFunctionDecl  Position QualIdent Int TypeExpr
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Module declarations
-- ---------------------------------------------------------------------------

data Decl
  = InfixDecl        Position Infix Integer [Ident]
  | DataDecl         Position Ident [Ident] [ConstrDecl]
  | NewtypeDecl      Position Ident [Ident] NewConstrDecl
  | TypeDecl         Position Ident [Ident] TypeExpr
  | TypeSig          Position [Ident] TypeExpr
  | EvalAnnot        Position [Ident] EvalAnnotation
  | FunctionDecl     Position Ident [Equation]
  | ExternalDecl     Position CallConv (Maybe String) Ident TypeExpr
  | FlatExternalDecl Position [Ident]
  | PatternDecl      Position ConstrTerm Rhs
  | ExtraVariables   Position [Ident]
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Infix declaration
-- ---------------------------------------------------------------------------

data Infix
  = InfixL
  | InfixR
  | Infix
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Data declaration
-- ---------------------------------------------------------------------------

data ConstrDecl
  = ConstrDecl Position [Ident] Ident [TypeExpr]
  | ConOpDecl  Position [Ident] TypeExpr Ident TypeExpr
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Newtype declaration
-- ---------------------------------------------------------------------------

data NewConstrDecl = NewConstrDecl Position [Ident] Ident TypeExpr
   deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Evaluation annotation
-- ---------------------------------------------------------------------------

data EvalAnnotation
  = EvalRigid
  | EvalChoice
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- C calling convention
-- ---------------------------------------------------------------------------

data CallConv
  = CallConvPrimitive
  | CallConvCCall
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Type expressions
-- ---------------------------------------------------------------------------

data TypeExpr
  = ConstructorType QualIdent [TypeExpr]
  | VariableType    Ident
  | TupleType       [TypeExpr]
  | ListType        TypeExpr
  | ArrowType       TypeExpr TypeExpr
  | RecordType      [([Ident], TypeExpr)] (Maybe TypeExpr)
    -- {l1 :: t1,...,ln :: tn | r}
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

data Equation = Equation Position Lhs Rhs
    deriving (Eq, Read, Show, Data, Typeable)

data Lhs
  = FunLhs Ident [ConstrTerm]
  | OpLhs  ConstrTerm Ident ConstrTerm
  | ApLhs  Lhs [ConstrTerm]
    deriving (Eq, Read, Show, Data, Typeable)

data Rhs
  = SimpleRhs  Position Expression [Decl]
  | GuardedRhs [CondExpr] [Decl]
    deriving (Eq, Read, Show, Data, Typeable)

data CondExpr = CondExpr Position Expression Expression
    deriving (Eq, Read, Show, Data, Typeable)

-- |Literals
-- The 'Ident' argument of an @Int@ literal is used for supporting ad-hoc
-- polymorphism on integer numbers. An integer literal can be used either as
-- an integer number or as a floating-point number depending on its context.
-- The compiler uses the identifier of the @Int@ literal for maintaining its
-- type.
data Literal
  = Char   SrcRef Char
  | Int    Ident  Integer
  | Float  SrcRef Double
  | String SrcRef String
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------------

data ConstrTerm
  = LiteralPattern     Literal
  | NegativePattern    Ident Literal
  | VariablePattern    Ident
  | ConstructorPattern QualIdent [ConstrTerm]
  | InfixPattern       ConstrTerm QualIdent ConstrTerm
  | ParenPattern       ConstrTerm
  | TuplePattern       SrcRef [ConstrTerm]
  | ListPattern        [SrcRef] [ConstrTerm]
  | AsPattern          Ident ConstrTerm
  | LazyPattern        SrcRef ConstrTerm
  | FunctionPattern    QualIdent [ConstrTerm]
  | InfixFuncPattern   ConstrTerm QualIdent ConstrTerm
  | RecordPattern      [Field ConstrTerm] (Maybe ConstrTerm)
        -- {l1 = p1, ..., ln = pn}  oder {l1 = p1, ..., ln = pn | p}
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

data Expression
  = Literal         Literal
  | Variable        QualIdent
  | Constructor     QualIdent
  | Paren           Expression
  | Typed           Expression TypeExpr
  | Tuple           SrcRef [Expression]
  | List            [SrcRef] [Expression]
  | ListCompr       SrcRef Expression [Statement] -- the ref corresponds to the main list
  | EnumFrom        Expression
  | EnumFromThen    Expression Expression
  | EnumFromTo      Expression Expression
  | EnumFromThenTo  Expression Expression Expression
  | UnaryMinus      Ident Expression
  | Apply           Expression Expression
  | InfixApply      Expression InfixOp Expression
  | LeftSection     Expression InfixOp
  | RightSection    InfixOp Expression
  | Lambda          SrcRef [ConstrTerm] Expression
  | Let             [Decl] Expression
  | Do              [Statement] Expression
  | IfThenElse      SrcRef Expression Expression Expression
  | Case            SrcRef Expression [Alt]
  | RecordConstr    [Field Expression]            -- {l1 = e1,...,ln = en}
  | RecordSelection Expression Ident              -- e -> l
  | RecordUpdate    [Field Expression] Expression -- {l1 := e1,...,ln := en | e}
    deriving (Eq, Read, Show, Data, Typeable)

data InfixOp
  = InfixOp     QualIdent
  | InfixConstr QualIdent
    deriving (Eq, Read, Show, Data, Typeable)

data Statement
  = StmtExpr SrcRef Expression
  | StmtDecl [Decl]
  | StmtBind SrcRef ConstrTerm Expression
    deriving (Eq, Read, Show, Data, Typeable)

data Alt = Alt Position ConstrTerm Rhs
    deriving (Eq, Read, Show, Data, Typeable)

data Field a = Field Position Ident a
    deriving (Eq, Read, Show, Data, Typeable)

instance SrcRefOf ConstrTerm where
  srcRefOf (LiteralPattern       l) = srcRefOf l
  srcRefOf (NegativePattern    i _) = srcRefOf i
  srcRefOf (VariablePattern      i) = srcRefOf i
  srcRefOf (ConstructorPattern i _) = srcRefOf i
  srcRefOf (InfixPattern     _ i _) = srcRefOf i
  srcRefOf (ParenPattern         c) = srcRefOf c
  srcRefOf (TuplePattern       s _) = s
  srcRefOf (ListPattern        _ _) = error "list pattern has several source refs"
  srcRefOf (AsPattern          i _) = srcRefOf i
  srcRefOf (LazyPattern        s _) = s
  srcRefOf (FunctionPattern    i _) = srcRefOf i
  srcRefOf (InfixFuncPattern _ i _) = srcRefOf i
  srcRefOf (RecordPattern      _ _) = error "record pattern has several source refs"

instance SrcRefOf Literal where
  srcRefOf (Char   s _) = s
  srcRefOf (Int    i _) = srcRefOf i
  srcRefOf (Float  s _) = s
  srcRefOf (String s _) = s
