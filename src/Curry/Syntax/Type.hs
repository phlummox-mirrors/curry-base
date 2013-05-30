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
    -- * Type classes
  , SContext (..), TypeConstructor (..), Context (..), ContextElem (..)
  , emptyContext
    -- * mirrored types
  , ConstrType_, Type_ (..), Context_ 
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
  | TypeSig      Position [Ident] Context TypeExpr               -- f, g :: Bool
  | FunctionDecl Position (Maybe ConstrType_) Ident [Equation]   -- f True = 1 ; f False = 0
  | ForeignDecl  Position CallConv (Maybe String) Ident TypeExpr -- foreign ccall "lib.h" fun :: Int
  | ExternalDecl Position [Ident]                                -- f, g external
  | PatternDecl  Position (Maybe ConstrType_) Pattern Rhs        -- Just x = ...
  | FreeDecl     Position [Ident]                                -- x, y free
  | ClassDecl    Position SContext Ident Ident [Decl]            -- class Eq a => Num a where {TypeSig|FunctionDecl|InfixDecl}
  | InstanceDecl Position SContext QualIdent TypeConstructor [Ident] [Decl] -- instance Foo a => Module1.Bar (Module2.TyCon a b c) where {FunctionDecl}
    deriving (Read, Show, Data, Typeable)

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
  -- TODO: merge ConstructorType + SpecialConstructorType
  | SpecialConstructorType TypeConstructor [TypeExpr]
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
  | ApLhs  Lhs [Pattern]         -- for example "(x $ y) z"
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
  | Variable        (Maybe ConstrType_) QualIdent
  | Constructor     QualIdent
  | Paren           Expression
  | Typed           Expression Context TypeExpr
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
    deriving (Read, Show, Data, Typeable)

-- |Infix operation
data InfixOp
  = InfixOp     (Maybe ConstrType_) QualIdent
  | InfixConstr QualIdent
    deriving (Read, Show, Data, Typeable)

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

-- ---------------------------------------------------------------------------
-- type classes
-- ---------------------------------------------------------------------------

-- |Simple class context, consisting of (classname, type variable) pairs
data SContext = SContext [(QualIdent, Ident)]
  deriving (Eq, Read, Show, Data, Typeable)

-- |Type constructors. Can be a qualified type constructor like "Prelude.Bool"
-- and the following special constructors: "()" "(,{,})" "[]" "(->)"
data TypeConstructor = QualTC QualIdent
                     | UnitTC
                     | TupleTC Int
                     | ListTC
                     | ArrowTC
  deriving (Eq, Read, Data, Typeable)
  
instance Show TypeConstructor where
  show (QualTC qid) = show qid
  show UnitTC = "()"
  show (TupleTC n) = "(" ++ replicate (n-1) ',' ++ ")"
  show ListTC = "[]"
  show ArrowTC = "(->)"
  
data Context = Context [ContextElem]
  deriving (Eq, Read, Show, Data, Typeable)

data ContextElem = ContextElem QualIdent Ident [TypeExpr]
  deriving (Eq, Read, Show, Data, Typeable)

emptyContext :: Context
emptyContext = Context []

-- ---------------------------------------------------------------------------
-- a mirror of the curry-frontend type Base.Types.Type etc. This is necessary
-- because cabal doesn't allow recursive depedencies, so we cannot 
-- refer to a type of curry-frontend in the abstract syntax tree. 
-- TODO: remove this again in the end (by parametrizing the AST)!!!
-- ---------------------------------------------------------------------------

type ConstrType_ = (Context_, Type_)

type Context_ = [(QualIdent, Type_)]

data Type_
  = TypeVariable_ Int
  | TypeConstructor_ QualIdent [Type_]
  | TypeArrow_ Type_ Type_
  | TypeConstrained_ [Type_] Int
  | TypeSkolem_ Int
  | TypeRecord_ [(Ident, Type_)] (Maybe Int)
  deriving (Eq, Read, Show, Data, Typeable)
  
-- ---------------------------------------------------------------------------
-- Eq instances for elements of the syntax tree (where needed). 
-- The equality functions should ignore the annotations in the syntax tree. 
-- ---------------------------------------------------------------------------

instance Eq Expression where
  (Literal l1) == (Literal l2) = l1 == l2
  (Variable _ q1) == (Variable _ q2) = q1 == q2
  (Constructor q1) == (Constructor q2) = q1 == q2
  (Paren e1) == (Paren e2) = e1 == e2
  (Typed e1 cx1 t1) == (Typed e2 cx2 t2) = e1 == e2 && cx1 == cx2 && t1 == t2
  (Tuple _ es1) == (Tuple _ es2) = es1 == es2
  (List _ es1) == (List _ es2) = es1 == es2
  (ListCompr _ e1 ss1) == (ListCompr _ e2 ss2) = e1 == e2 && ss1 == ss2
  (EnumFrom e1) == (EnumFrom e2) = e1 == e2
  (EnumFromThen e11 e21) == (EnumFromThen e12 e22) = e11 == e12 && e21 == e22
  (EnumFromTo e11 e21) == (EnumFromTo e12 e22) = e11 == e12 && e21 == e22
  (EnumFromThenTo e11 e21 e31) == (EnumFromThenTo e12 e22 e32) 
    = e11 == e12 && e21 == e22 && e31 == e32
  (UnaryMinus i1 e1) == (UnaryMinus i2 e2) = i1 == i2 && e1 == e2
  (Apply e11 e21) == (Apply e12 e22) = e11 == e12 && e21 == e22
  (InfixApply e11 op1 e21) == (InfixApply e12 op2 e22) 
    =  e11 == e12 && op1 == op2 && e21 == e22
  (LeftSection e1 op1) == (LeftSection e2 op2) = e1 == e2 && op1 == op2
  (RightSection op1 e1) == (RightSection op2 e2) = op1 == op2 && e1 == e2
  (Lambda _ ps1 e1) == (Lambda _ ps2 e2) = ps1 == ps2 && e1 == e2
  (Let ds1 e1) == (Let ds2 e2) = ds1 == ds2 && e1 == e2
  (Do ss1 e1) == (Do ss2 e2) = ss1 == ss2 && e1 == e2
  (IfThenElse _ e11 e21 e31) == (IfThenElse _ e12 e22 e32) 
    = e11 == e12 && e21 == e22 && e31 == e32
  (Case _ ct1 e1 as1) == (Case _ ct2 e2 as2) = ct1 == ct2 && e1 == e2 && as1 == as2
  (RecordConstr fs1) == (RecordConstr fs2) = fs1 == fs2
  (RecordSelection e1 i1) == (RecordSelection e2 i2) = e1 == e2 && i1 == i2
  (RecordUpdate fs1 e1) == (RecordUpdate fs2 e2) = fs1 == fs2 && e1 == e2
  _ == _ = False

instance Eq Decl where
  (InfixDecl p1 f1 i1 ids1) == (InfixDecl p2 f2 i2 ids2) 
    = p1 == p2 && f1 == f2 && i1 == i2 && ids1 == ids2
  (DataDecl p1 i1 ids1 cs1) == (DataDecl p2 i2 ids2 cs2) 
    = p1 == p2 && i1 == i2 && ids1 == ids2 && cs1 == cs2
  (NewtypeDecl p1 i1 ids1 n1) == (NewtypeDecl p2 i2 ids2 n2) 
    = p1 == p2 && i1 == i2 && ids1 == ids2 && n1 == n2
  (TypeDecl p1 id1 ids1 t1) == (TypeDecl p2 id2 ids2 t2) 
    = p1 == p2 && id1 == id2 && ids1 == ids2 && t1 == t2
  (TypeSig p1 ids1 cx1 t1) == (TypeSig p2 ids2 cx2 t2) 
    = p1 == p2 && ids1 == ids2 && cx1 == cx2 && t1 == t2
  (FunctionDecl p1 _ id1 es1) == (FunctionDecl p2 _ id2 es2) 
    = p1 == p2 && id1 == id2 && es1 == es2
  (ForeignDecl p1 c1 s1 i1 t1) == (ForeignDecl p2 c2 s2 i2 t2) 
    = p1 == p2 && c1 == c2 && s1 == s2 && i1 == i2 && t1 == t2
  (ExternalDecl p1 is1) == (ExternalDecl p2 is2) = p1 == p2 && is1 == is2
  (PatternDecl p1 _ pt1 r1) == (PatternDecl p2 _ pt2 r2) 
    = p1 == p2 && pt1 == pt2 && r1 == r2
  (FreeDecl p1 is1) == (FreeDecl p2 is2) = p1 == p2 && is1 == is2
  (ClassDecl p1 s1 c1 a1 ds1) == (ClassDecl p2 s2 c2 a2 ds2) 
    = p1 == p2 && s1 == s2 && c1 == c2 && a1 == a2 && ds1 == ds2
  (InstanceDecl p1 s1 c1 t1 is1 ds1) == (InstanceDecl p2 s2 c2 t2 is2 ds2)
    = p1 == p2 && s1 == s2 && c1 == c2 && t1 == t2 && is1 == is2 && ds1 == ds2
  _ == _ = False

instance Eq InfixOp where
  (InfixOp _ op1) == (InfixOp _ op2) = op1 == op2
  (InfixConstr op1) == (InfixConstr op2) = op1 == op2
  _ == _ = False
  