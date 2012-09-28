{- |
    Module      :  $Header$
    Description :  Abstract syntax for Curry
    Copyright   :  (c) 1999 - 2004 Wolfgang Lux
                       2005        Martin Engelke
                       2011 - 2012 Björn Peemöller
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
  , CallConv (..), TypeExpr (..)
  , Equation (..), Lhs (..), Rhs (..), CondExpr (..)
  , Literal (..), Pattern (..), Expression (..), InfixOp (..)
  , Statement (..), CaseType (..), Alt (..), Field (..)
    -- * Goals
  , Goal (..)
  ) where

import Data.Generics (Data (..), Typeable (..))

import Curry.Base.Ident
import Curry.Base.Position

-- ---------------------------------------------------------------------------
-- Modules
-- ---------------------------------------------------------------------------

-- |Curry module
data Module = Module ModuleIdent (Maybe ExportSpec) [ImportDecl] [Decl]
    deriving (Eq, Read, Show, Data, Typeable)

-- |Export specification
data ExportSpec = Exporting Position [Export]
    deriving (Eq, Read, Show, Data, Typeable)

-- |Single exported entity
data Export
  = Export         QualIdent         -- f/T
  | ExportTypeWith QualIdent [Ident] -- T (C1,...,Cn)
  | ExportTypeAll  QualIdent         -- T (..)
  | ExportModule   ModuleIdent       -- module M
    deriving (Eq, Read, Show, Data, Typeable)

-- |Import declaration
data ImportDecl = ImportDecl Position ModuleIdent Qualified
                             (Maybe ModuleIdent) (Maybe ImportSpec)
    deriving (Eq, Read, Show, Data, Typeable)

-- |Flag to signal qualified import
type Qualified = Bool

-- |Import specification
data ImportSpec
  = Importing Position [Import]
  | Hiding    Position [Import]
    deriving (Eq, Read, Show, Data, Typeable)

-- |Single imported entity
data Import
  = Import         Ident            -- f/T
  | ImportTypeWith Ident [Ident]    -- T (C1,...,Cn)
  | ImportTypeAll  Ident            -- T (..)
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Module interfaces
-- ---------------------------------------------------------------------------

-- | Module interface
--
-- Interface declarations are restricted to type declarations and signatures.
-- Note that an interface function declaration additionaly contains the
-- function arity (= number of parameters) in order to generate
-- correct FlatCurry function applications.
data Interface = Interface ModuleIdent [IImportDecl] [IDecl]
    deriving (Eq, Read, Show, Data, Typeable)

-- |Interface import declaration
data IImportDecl = IImportDecl Position ModuleIdent
    deriving (Eq, Read, Show, Data, Typeable)

-- |Interface declaration
data IDecl
  = IInfixDecl     Position Infix Integer QualIdent -- TODO: change to int?
  | HidingDataDecl Position Ident [Ident]           -- TODO: change to QualIdent?
  | IDataDecl      Position QualIdent [Ident] [Maybe ConstrDecl]
  | INewtypeDecl   Position QualIdent [Ident] NewConstrDecl
  | ITypeDecl      Position QualIdent [Ident] TypeExpr
  | IFunctionDecl  Position QualIdent Int TypeExpr
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Declarations (local or top-level)
-- ---------------------------------------------------------------------------

-- |Declaration in a module
data Decl
  = InfixDecl    Position Infix Integer [Ident]                  -- infixl 5 (op), `fun` -- TODO: Make precedence optional and change to int
  | DataDecl     Position Ident [Ident] [ConstrDecl]             -- data C a b = C1 a | C2 b
  | NewtypeDecl  Position Ident [Ident] NewConstrDecl            -- newtype C a b = C a b
  | TypeDecl     Position Ident [Ident] TypeExpr                 -- type C a b = D a b
  | TypeSig      Position [Ident] TypeExpr                       -- f, g :: Bool
  | FunctionDecl Position Ident [Equation]                       -- f True = 1 ; f False = 0
  | ForeignDecl  Position CallConv (Maybe String) Ident TypeExpr -- foreign ccall "lib.h" fun :: Int
  | ExternalDecl Position [Ident]                                -- f, g external
  | PatternDecl  Position Pattern Rhs                            -- Just x = ...
  | FreeDecl     Position [Ident]                                -- x, y free
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Infix declaration
-- ---------------------------------------------------------------------------

-- |Fixity of operators
data Infix
  = InfixL -- ^ left-associative
  | InfixR -- ^ right-associative
  | Infix  -- ^ no associativity
    deriving (Eq, Read, Show, Data, Typeable)

-- |Constructor declaration for algebraic data types
data ConstrDecl
  = ConstrDecl Position [Ident] Ident [TypeExpr]
  | ConOpDecl  Position [Ident] TypeExpr Ident TypeExpr
    deriving (Eq, Read, Show, Data, Typeable)

-- |Constructor declaration for renaming types (newtypes)
data NewConstrDecl = NewConstrDecl Position [Ident] Ident TypeExpr
   deriving (Eq, Read, Show, Data, Typeable)

-- |Calling convention for C code
data CallConv
  = CallConvPrimitive
  | CallConvCCall
    deriving (Eq, Read, Show, Data, Typeable)

-- |Type expressions
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

-- |Function defining equation
data Equation = Equation Position Lhs Rhs
    deriving (Eq, Read, Show, Data, Typeable)

-- |Left-hand-side of an 'Equation' (function identifier and patterns)
data Lhs
  = FunLhs Ident [Pattern]       -- f x y
  | OpLhs  Pattern Ident Pattern -- x $ y
  | ApLhs  Lhs [Pattern]         -- ($) x y
    deriving (Eq, Read, Show, Data, Typeable)

-- |Right-hand-side of an 'Equation'
data Rhs
  = SimpleRhs  Position Expression [Decl] -- @expr where decls@
  | GuardedRhs [CondExpr] [Decl]          -- @| cond = expr where decls@
    deriving (Eq, Read, Show, Data, Typeable)

-- |Conditional expression (expression conditioned by a guard)
data CondExpr = CondExpr Position Expression Expression
    deriving (Eq, Read, Show, Data, Typeable)

-- |Literal
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

-- |Constructor term (used for patterns)
data Pattern
  = LiteralPattern     Literal
  | NegativePattern    Ident Literal
  | VariablePattern    Ident
  | ConstructorPattern QualIdent [Pattern]
  | InfixPattern       Pattern QualIdent Pattern
  | ParenPattern       Pattern
  | TuplePattern       SrcRef [Pattern]
  | ListPattern        [SrcRef] [Pattern]
  | AsPattern          Ident Pattern
  | LazyPattern        SrcRef Pattern
  | FunctionPattern    QualIdent [Pattern]
  | InfixFuncPattern   Pattern QualIdent Pattern
  | RecordPattern      [Field Pattern] (Maybe Pattern)
        -- {l1 = p1, ..., ln = pn | p}
    deriving (Eq, Read, Show, Data, Typeable)

-- |Expression
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
  | Lambda          SrcRef [Pattern] Expression
  | Let             [Decl] Expression
  | Do              [Statement] Expression
  | IfThenElse      SrcRef Expression Expression Expression
  | Case            SrcRef CaseType Expression [Alt]
  | RecordConstr    [Field Expression]            -- {l1 := e1,...,ln := en}
  | RecordSelection Expression Ident              -- e :> l
  | RecordUpdate    [Field Expression] Expression -- {l1 := e1,...,ln := en | e}
    deriving (Eq, Read, Show, Data, Typeable)

-- |Infix operation
data InfixOp
  = InfixOp     QualIdent
  | InfixConstr QualIdent
    deriving (Eq, Read, Show, Data, Typeable)

-- |Statement (used for do-sequence and list comprehensions)
data Statement
  = StmtExpr SrcRef Expression
  | StmtDecl [Decl]
  | StmtBind SrcRef Pattern Expression
    deriving (Eq, Read, Show, Data, Typeable)

-- |Type of case expressions
data CaseType
  = Rigid
  | Flex
    deriving (Eq, Read, Show, Data, Typeable)

-- |Single case alternative
data Alt = Alt Position Pattern Rhs
    deriving (Eq, Read, Show, Data, Typeable)

-- |Record field
data Field a = Field Position Ident a
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- Goals
-- ---------------------------------------------------------------------------

-- |Goal in REPL (expression to evaluate)
data Goal = Goal Position Expression [Decl]
    deriving (Eq, Read, Show, Data, Typeable)

-- ---------------------------------------------------------------------------
-- instances
-- ---------------------------------------------------------------------------

instance SrcRefOf Pattern where
  srcRefOf (LiteralPattern       l) = srcRefOf l
  srcRefOf (NegativePattern    i _) = srcRefOf i
  srcRefOf (VariablePattern      i) = srcRefOf i
  srcRefOf (ConstructorPattern i _) = srcRefOf i
  srcRefOf (InfixPattern     _ i _) = srcRefOf i
  srcRefOf (ParenPattern         c) = srcRefOf c
  srcRefOf (TuplePattern       s _) = s
  srcRefOf (ListPattern        _ _)
    = error "list pattern has several source refs"
  srcRefOf (AsPattern          i _) = srcRefOf i
  srcRefOf (LazyPattern        s _) = s
  srcRefOf (FunctionPattern    i _) = srcRefOf i
  srcRefOf (InfixFuncPattern _ i _) = srcRefOf i
  srcRefOf (RecordPattern      _ _)
    = error "record pattern has several source refs"

instance SrcRefOf Literal where
  srcRefOf (Char   s _) = s
  srcRefOf (Int    i _) = srcRefOf i
  srcRefOf (Float  s _) = s
  srcRefOf (String s _) = s
