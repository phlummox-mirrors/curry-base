{- |
    Module      :  $Header$
    Description :  Library to support meta-programming in Curry
    Copyright   :  Michael Hanus  , April 2004
                   Martin Engelke , July 2005
                   Björn Peemöller, November 2013
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This library contains a definition for representing Curry programs
    in Haskell by the type 'CurryProg' and I/O actions to read Curry programs
    and transform them into this abstract representation as well as
    write them to a file.

    Note that this defines a slightly new format for AbstractCurry
    in comparison to the first proposal of 2003.

    /Assumption:/ An AbstractCurry program @Prog@ is stored in a file with
    the file extension @acy@, i.e. in a file @Prog.acy@.
-}
module Curry.AbstractCurry.Type
  ( CurryProg (..), QName, CLabel, CVisibility (..), CTVarIName
  , CTypeDecl (..), CConsDecl (..), CTypeExpr (..), COpDecl (..), CFixity (..)
  , CVarIName, CFuncDecl (..), CRhs (..), CRule (..)
  , CLocalDecl (..), CExpr (..), CCaseType (..), CStatement (..)
  , CPattern (..), CLiteral (..), CField
  ) where

-- ---------------------------------------------------------------------------
-- Abstract syntax
-- ---------------------------------------------------------------------------

{- |A qualified name.

    In AbstractCurry all names are qualified to avoid name clashes.
    The first component is the module name and the second component the
    unqualified name as it occurs in the source program.
-}
type QName = (String, String)


-- |Identifiers for record labels (extended syntax).
type CLabel = String


-- |Data type to specify the visibility of various entities.
data CVisibility
  = Public    -- ^ exported entity
  | Private   -- ^ private entity
    deriving (Eq, Read, Show)


{- |A Curry module in the intermediate form. A value of this type has the form

    @CurryProg modname imports typedecls funcdecls opdecls@

    where

    [@modname@]   Name of this module

    [@imports@]   List of modules names that are imported

    [@typedecls@] Type declarations

    [@funcdecls@] Function declarations

    [@ opdecls@]  Operator precedence declarations
-}
data CurryProg = CurryProg String [String] [CTypeDecl] [CFuncDecl] [COpDecl]
    deriving (Eq, Read, Show)


{- |Definitions of algebraic data types and type synonyms.

    A data type definition of the form

    @data t x1...xn = ...| c t1....tkc |...@

    is represented by the Curry term

    @(CType t v [i1,...,in] [...(CCons c kc v [t1,...,tkc])...])@

    where each @ij@ is the index of the type variable @xj@ and @v@ is the
    visibility of the type resp. constructor.

    /Note:/ The type variable indices are unique inside each type declaration
            and are usually numbered from 0.

    Thus, a data type declaration consists of the name of the data type,
    a list of type parameters and a list of constructor declarations.
-}
data CTypeDecl
  = CType QName CVisibility [CTVarIName] [CConsDecl]  -- ^ algebraic data type
  | CTypeSyn QName CVisibility [CTVarIName] CTypeExpr -- ^ type synonym
    deriving (Eq, Read, Show)


{- |The type for representing type variables.

    They are represented by @(i,n)@ where @i@ is a type variable index
    which is unique inside a function and @n@ is a name (if possible,
    the name written in the source program).
-}
type CTVarIName = (Int, String)


{- |A constructor declaration consists of the name and arity of the
    constructor and a list of the argument types of the constructor.
-}
data CConsDecl = CCons QName Int CVisibility [CTypeExpr]
    deriving (Eq, Read, Show)


{- |Type expression.

    A type expression is either a type variable, a function type,
    or a type constructor application.

    /Note:/ the names of the predefined type constructors are
            @Int@, @Float@, @Bool@, @Char@, @IO@, @Success@,
            @()@ (unit type), @(,...,)@ (tuple types), @[]@ (list type)
-}
data CTypeExpr
    -- |Type variable
  = CTVar CTVarIName
    -- |Function type @t1 -> t2@
  | CFuncType CTypeExpr CTypeExpr
    -- |Type constructor application
  | CTCons QName [CTypeExpr]
    -- |Record type (extended Curry)
  | CRecordType [CField CTypeExpr] (Maybe CTVarIName)
    deriving (Eq, Read, Show)


-- |Labeled record fields
type CField a = (CLabel, a)


{- |Operator precedence declaration.

    An operator precedence declaration @fix p n@ in Curry corresponds to the
    AbstractCurry term @(COp n fix p)@.
-}
data COpDecl = COp QName CFixity Int
    deriving (Eq, Read, Show)


-- |Fixity declarations of infix operators
data CFixity
  = CInfixOp  -- ^ non-associative infix operator
  | CInfixlOp -- ^ left-associative infix operator
  | CInfixrOp -- ^ right-associative infix operator
    deriving (Eq, Read, Show)


{- |Data type for representing function declarations.

    A function declaration in FlatCurry is a term of the form

    @(CFunc name arity visibility type (CRules eval [CRule rule1,...,rulek]))@

    and represents the function @name@ with definition

    @
    name :: type
    rule1
    ...
    rulek
    @

    /Note:/ The variable indices are unique inside each rule.

    External functions are represented as

    @(CFunc name arity type (CExternal s))@

    where s is the external name associated to this function.

    Thus, a function declaration consists of the name, arity, type, and
    a list of rules.

    If the list of rules is empty the function is considered to be externally defined.
-}
data CFuncDecl = CFunc QName Int CVisibility CTypeExpr [CRule]
    deriving (Eq, Read, Show)


{- |The general form of a function rule. It consists of a list of patterns
    (left-hand side), a list of guards (@success@ if not present in the
    source text) with their corresponding right-hand sides, and
    a list of local declarations.
-}
data CRule = CRule [CPattern] CRhs
    deriving (Eq, Read, Show)


-- |Right-hand-side of a 'CRule' or an @case@ expression
data CRhs
  = CSimpleRhs  CExpr [CLocalDecl] -- @expr where decls@
  | CGuardedRhs [(CExpr, CExpr)] [CLocalDecl]          -- @| cond = expr where decls@
    deriving (Eq, Read, Show)


-- | Local (let/where) declarations
data CLocalDecl
  = CLocalFunc CFuncDecl                  -- ^ local function declaration
  | CLocalPat CPattern CExpr [CLocalDecl] -- ^ local pattern declaration
  | CLocalVar CVarIName                   -- ^ local free variable declaration
    deriving (Eq, Read, Show)


{- |Object variables.

    Object variables occurring in expressions are represented by @(Var i)@
    where @i@ is a variable index.
-}
type CVarIName = (Int, String)


-- |Pattern expressions.
data CPattern
    -- |pattern variable (unique index / name)
  = CPVar CVarIName
    -- |literal (Integer/Float/Char constant)
  | CPLit CLiteral
    -- |application @(m.c e1 ... en)@ of n-ary constructor @m.c@
    --  (@CPComb (m,c) [e1,...,en]@)
  | CPComb QName [CPattern]
    -- |as-pattern (extended Curry)
  | CPAs CVarIName CPattern
    -- |functional pattern (extended Curry)
  | CPFuncComb QName [CPattern]
    -- |lazy pattern (extended Curry)
  | CPLazy CPattern
    -- |record pattern (extended curry)
  | CPRecord [CField CPattern] (Maybe CPattern)
    deriving (Eq, Read, Show)


-- | Curry expressions.
data CExpr
  = CVar       CVarIName            -- ^ variable (unique index / name)
  | CLit       CLiteral             -- ^ literal (Integer/Float/Char constant)
  | CSymbol    QName                -- ^ a defined symbol with module and name
  | CApply     CExpr CExpr          -- ^ application (e1 e2)
  | CLambda    [CPattern] CExpr     -- ^ lambda abstraction
  | CLetDecl   [CLocalDecl] CExpr   -- ^ local let declarations
  | CDoExpr    [CStatement]         -- ^ do expression
  | CListComp  CExpr [CStatement]   -- ^ list comprehension
  | CCase      CCaseType CExpr [(CPattern, CRhs)]  -- ^ case expression
  | CTyped     CExpr CTypeExpr      -- ^ typed expression
  | CRecConstr [CField CExpr]       -- ^ record construction (extended Curry)
  | CRecSelect CExpr CLabel         -- ^ field selection (extended Curry)
  | CRecUpdate [CField CExpr] CExpr -- ^ record update (extended Curry)
    deriving (Eq, Read, Show)


{- |Literals occurring in an expression, either an integer, a float,
    or a character constant.

    /Note:/ The constructor definition of 'CIntc' differs from the original
    PAKCS definition. It uses Haskell type 'Integer' instead of 'Int'
    to provide an unlimited range of integer numbers. Furthermore,
    float values are represented with Haskell type 'Double' instead of
    'Float' to gain double precision.
-}
data CLiteral
  = CIntc   Integer -- ^ Int literal
  | CFloatc Double  -- ^ Float literal
  | CCharc  Char    -- ^ Char literal
  | CString String  -- ^ String literal
    deriving (Eq, Read, Show)


-- |Statements in do expressions and list comprehensions.
data CStatement
  = CSExpr CExpr         -- ^ an expression (I/O action or boolean)
  | CSPat CPattern CExpr -- ^ a pattern definition
  | CSLet [CLocalDecl]   -- ^ a local let declaration
    deriving (Eq, Read, Show)


-- |Type of case expressions
data CCaseType
  = CRigid -- ^ rigid case expression
  | CFlex  -- ^ flexible case expression
    deriving (Eq, Read, Show)
