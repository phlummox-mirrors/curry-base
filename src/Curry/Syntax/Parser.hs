{- |
    Module      :  $Header$
    Description :  A Parser for Curry
    Copyright   :  (c) 1999-2004 Wolfgang Lux
                       2005 Martin Engelke
                       2011 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    The Curry parser is implemented using the (mostly) LL(1) parsing
    combinators described in appendix~\ref{sec:ll-parsecomb}.
-}

module Curry.Syntax.Parser (parseSource, parseHeader, parseInterface) where

import Curry.Base.Ident
import Curry.Base.Position
import Curry.Base.MessageMonad
import Curry.Base.LLParseComb

import Curry.Syntax.Lexer (Token (..), Category (..), Attributes (..), lexer)
import Curry.Syntax.Type
import Curry.Syntax.Utils (mk, mk', mkInt, addSrcRefs)

-- |Parse a module
parseSource :: Bool -> FilePath -> String -> MsgMonad Module
parseSource flat path = fmap addSrcRefs
                      . applyParser (moduleHeader <*> decls flat) lexer path

-- |Parse a module header
parseHeader :: FilePath -> String -> MsgMonad Module
parseHeader = prefixParser (moduleHeader <*> succeed []) lexer

-- |Parse an interface
parseInterface :: FilePath -> String -> MsgMonad Interface
parseInterface = applyParser interface lexer

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

-- |Parser for a module header
moduleHeader :: Parser Token ([Decl] -> Module) a
moduleHeader =  Module <$-> token KW_module
                       <*>  (mIdent <?> "module name expected")
                       <*>  ((Just <$> exportSpec) `opt` Nothing)
                       <*-> (token KW_where <?> "where expected")
                       <*>  importDecls
            <|> Module mainMIdent Nothing <$> importDecls

-- |Parser for an export specification
exportSpec :: Parser Token ExportSpec a
exportSpec = Exporting <$> position <*> parens (export `sepBy` comma)

-- |Parser for an export item
export :: Parser Token Export a
export =  qtycon <**> (parens spec `opt` Export)
      <|> Export <$> qfun <\> qtycon
      <|> ExportModule <$-> token KW_module <*> mIdent
  where spec =  ExportTypeAll <$-> token DotDot
            <|> flip ExportTypeWith <$> con `sepBy` comma

-- |Parser for import declarations
importDecls :: Parser Token [ImportDecl] a
importDecls =   (leftBrace `opt` undefined)
           <-*> many (importDecl <*-> many semicolon)

-- |Parser for a single import declaration
importDecl :: Parser Token ImportDecl a
importDecl =  flip . ImportDecl
          <$> position <*-> token KW_import
          <*> (True <$-> token Id_qualified `opt` False)
          <*> mIdent
          <*> (Just <$-> token Id_as <*> mIdent `opt` Nothing)
          <*> (Just <$> importSpec `opt` Nothing)

-- |Parser for an import specification
importSpec :: Parser Token ImportSpec a
importSpec =   position
          <**> (Hiding <$-> token Id_hiding `opt` Importing)
          <*>  parens (spec `sepBy` comma)
  where
    spec    =  tycon <**> (parens constrs `opt` Import)
           <|> Import <$> fun <\> tycon
    constrs =  ImportTypeAll <$-> token DotDot
           <|> flip ImportTypeWith <$> con `sepBy` comma

-- ---------------------------------------------------------------------------
-- Interfaces
-- ---------------------------------------------------------------------------

-- |Parser for an interface
interface :: Parser Token Interface a
interface =   Interface
         <$-> token Id_interface
         <*>  (mIdent <?> "module name expected")
         <*-> (token KW_where <?> "where expected")
         <*-> leftBrace
         <*>  iImportDecls
         <*>  intfDecls
         <*-> rightBrace

-- |Parser for interface import declarations
iImportDecls :: Parser Token [IImportDecl] a
iImportDecls = iImportDecl `sepBy` semicolon

-- |Parser for a single interface import declaration
iImportDecl :: Parser Token IImportDecl a
iImportDecl = IImportDecl <$> position <*-> token KW_import <*> mIdent

-- |Parser for interface declarations
intfDecls :: Parser Token [IDecl] a
intfDecls = intfDecl `sepBy` semicolon

-- |Parser for a single interface declaration
intfDecl :: Parser Token IDecl a
intfDecl =  iInfixDecl <|> iHidingDecl <|> iDataDecl <|> iNewtypeDecl
        <|> iTypeDecl  <|> iFunctionDecl <\> token Id_hiding

-- |Parser for an interface infix declaration
iInfixDecl :: Parser Token IDecl a
iInfixDecl = infixDeclLhs IInfixDecl <*> qfunop

-- |Parser for an interface hiding declaration
iHidingDecl :: Parser Token IDecl a
iHidingDecl = position <*-> token Id_hiding <**> (hDataDecl <|> hFuncDecl)
  where hDataDecl = hiddenData <$-> token KW_data     <*> tycon <*> many tyvar
        hFuncDecl = hidingFunc <$-> token DoubleColon <*> type0
        hiddenData tc tvs p = HidingDataDecl p tc tvs
        -- TODO: 0 was inserted to type check, but what is the meaning of this field?
        hidingFunc ty p = IFunctionDecl p hidingId 0 ty
        hidingId = qualify (mkIdent "hiding")

-- |Parser for an interface data declaration
iDataDecl :: Parser Token IDecl a
iDataDecl = iTypeDeclLhs IDataDecl KW_data <*-> equals <*> constrs
  where constrs = iConstrDecl `sepBy1` bar `opt` []
        iConstrDecl =  Just    <$>  constrDecl <\> token Underscore
                   <|> Nothing <$-> token Underscore

-- |Parser for an interface newtype declaration
iNewtypeDecl :: Parser Token IDecl a
iNewtypeDecl = iTypeDeclLhs INewtypeDecl KW_newtype <*-> equals <*> newConstrDecl

-- |Parser for an interface type synonym declaration
iTypeDecl :: Parser Token IDecl a
iTypeDecl = iTypeDeclLhs ITypeDecl KW_type <*-> equals <*> type0

-- |Parser for an interface function declaration
iFunctionDecl :: Parser Token IDecl a
iFunctionDecl = IFunctionDecl <$> position <*> qfun <*-> token DoubleColon
              <*> succeed 0 <*> type0

iTypeDeclLhs :: (Position -> QualIdent -> [Ident] -> a) -> Category
             -> Parser Token a b
iTypeDeclLhs f kw = f <$> position <*-> token kw <*> qtycon <*> many tyvar

-- ---------------------------------------------------------------------------
-- Declarations
-- ---------------------------------------------------------------------------

decls :: Bool -> Parser Token [Decl] a
decls = layout . globalDecls

globalDecls :: Bool -> Parser Token [Decl] a
globalDecls flat = topDecl flat `sepBy` semicolon

topDecl :: Bool -> Parser Token Decl a
topDecl flat
  | flat      =  infixDecl <|> dataDecl                 <|> typeDecl
             <|> functionDecl flat
  | otherwise =  infixDecl <|> dataDecl <|> newtypeDecl <|> typeDecl
             <|> functionDecl flat <|> externalDecl

infixDecl :: Parser Token Decl a
infixDecl = infixDeclLhs InfixDecl <*> funop `sepBy1` comma

infixDeclLhs :: (Position -> Infix -> Integer -> a) -> Parser Token a b
infixDeclLhs f = f <$> position <*> tokenOps infixKW <*> integer
  where infixKW = [(KW_infix, Infix), (KW_infixl, InfixL), (KW_infixr, InfixR)]

dataDecl :: Parser Token Decl a
dataDecl = typeDeclLhs DataDecl KW_data <*> constrs
  where constrs = equals <-*> constrDecl `sepBy1` bar `opt` []

newtypeDecl :: Parser Token Decl a
newtypeDecl = typeDeclLhs NewtypeDecl KW_newtype <*-> equals <*> newConstrDecl

typeDecl :: Parser Token Decl a
typeDecl = typeDeclLhs TypeDecl KW_type <*-> equals <*> typeDeclRhs --was type0
  where typeDeclRhs = type0 <|> recordDecl

typeDeclLhs :: (Position -> Ident -> [Ident] -> a) -> Category
            -> Parser Token a b
typeDeclLhs f kw = f <$> position <*-> token kw <*> tycon <*> many typeVar
  where typeVar = tyvar <|> anonIdent

constrDecl :: Parser Token ConstrDecl a
constrDecl = position <**> (existVars <**> constr)
  where constr =  conId     <**> identDecl
              <|> leftParen <-*> parenDecl
              <|> type1 <\> conId <\> leftParen <**> opDecl
        identDecl = many type2 <**> (conType <$> opDecl `opt` conDecl)
        parenDecl =  conOpDeclPrefix
                 <$> conSym <*-> rightParen <*> type2 <*> type2
                 <|> tupleType <*-> rightParen <**> opDecl
        opDecl = conOpDecl <$> conop <*> type1
        conType f tys c                  = f $ ConstructorType (qualify c) tys
        conDecl tys c tvs p              = ConstrDecl p tvs c tys
        conOpDecl op ty2 ty1 tvs p       = ConOpDecl p tvs ty1 op ty2
        conOpDeclPrefix op ty1 ty2 tvs p = ConOpDecl p tvs ty1 op ty2

newConstrDecl :: Parser Token NewConstrDecl a
newConstrDecl =
  NewConstrDecl <$> position <*> existVars <*> con <*> type2

recordDecl :: Parser Token TypeExpr a
recordDecl =  flip RecordType Nothing
          <$> (layoutOff <-*> braces (labelDecls `sepBy` comma))

labelDecls :: Parser Token ([Ident], TypeExpr) a
labelDecls = (,) <$> labId `sepBy1` comma <*-> token DoubleColon <*> type0

valueDecls :: Bool -> Parser Token [Decl] a
valueDecls flat = localDecl `sepBy` semicolon
  where localDecl
          | flat      = infixDecl <|> valueDecl flat
          | otherwise = infixDecl <|> valueDecl flat <|> externalDecl

existVars :: Parser Token [Ident] a
{-
existVars flat
  | flat = succeed []
  | otherwise = token Id_forall <-*> many1 tyvar <*-> dot `opt` []
-}
existVars = succeed []

functionDecl :: Bool -> Parser Token Decl a
functionDecl flat = position <**> decl
  where decl = fun `sepBy1` comma <**> funListDecl flat
          <|?> funDecl <$> lhs <*> declRhs flat
        lhs = (\f -> (f, FunLhs f [])) <$> fun
         <|?> funLhs

valueDecl :: Bool -> Parser Token Decl a
valueDecl flat = position <**> decl
  where decl =   var `sepBy1` comma <**> valListDecl flat
            <|?> valDecl <$> constrTerm0 <*> declRhs flat
            <|?> funDecl <$> curriedLhs <*> declRhs flat
        valDecl (ConstructorPattern c ts)
          | not (isConstrId c) = funDecl (f,FunLhs f ts)
          where f = unqualify c
        valDecl t = opDecl id t
        opDecl f (InfixPattern t1 op t2)
          | isConstrId op = opDecl (f . InfixPattern t1 op) t2
          | otherwise    = funDecl (op', OpLhs (f t1) op' t2)
          where op' = unqualify op
        opDecl f t = patDecl (f t)
        isConstrId c = c == qConsId || isQualified c || isQTupleId c

funDecl :: (Ident,Lhs) -> Rhs -> Position -> Decl
funDecl (f,lhs) rhs' p = FunctionDecl p f [Equation p lhs rhs']

patDecl :: ConstrTerm -> Rhs -> Position -> Decl
patDecl t rhs' p = PatternDecl p t rhs'

funListDecl :: Bool -> Parser Token ([Ident] -> Position -> Decl) a
funListDecl flat
  | flat      =  typeSig       <$-> token DoubleColon <*> type0
             <|> evalAnnot     <$-> token KW_eval     <*> tokenOps evalKW
             <|> externalDecl' <$-> token KW_external
  | otherwise =  typeSig       <$-> token DoubleColon <*> type0
             <|> evalAnnot     <$-> token KW_eval     <*> tokenOps evalKW
  where typeSig ty vs p    = TypeSig p vs ty
        evalAnnot ev vs p  = EvalAnnot p vs ev
        evalKW             = [(KW_rigid, EvalRigid), (KW_choice, EvalChoice)]
        externalDecl' vs p = FlatExternalDecl p vs

valListDecl :: Bool -> Parser Token ([Ident] -> Position -> Decl) a
valListDecl flat = funListDecl flat <|> extraVars <$-> token KW_free
  where extraVars vs p = ExtraVariables p vs

funLhs :: Parser Token (Ident,Lhs) a
funLhs = funLhs' <$> fun <*> many1 constrTerm2
    <|?> flip ($ id) <$> constrTerm1 <*> opLhs'
    <|?> curriedLhs
  where opLhs' = opLhs <$> funSym <*> constrTerm0
             <|> infixPat <$> gConSym <\> funSym <*> constrTerm1 <*> opLhs'
             <|> backquote <-*> opIdLhs
        opIdLhs = opLhs <$> funId <*-> checkBackquote <*> constrTerm0
              <|> infixPat <$> qConId <\> funId <*-> backquote <*> constrTerm1
                           <*> opLhs'
        funLhs' f ts = (f,FunLhs f ts)
        opLhs op t2 f t1 = (op,OpLhs (f t1) op t2)
        infixPat op t2 f g t1 = f (g . InfixPattern t1 op) t2

curriedLhs :: Parser Token (Ident,Lhs) a
curriedLhs = apLhs <$> parens funLhs <*> many1 constrTerm2
  where apLhs (f,lhs) ts = (f,ApLhs lhs ts)

declRhs :: Bool -> Parser Token Rhs a
declRhs flat = rhs flat equals

rhs :: Bool -> Parser Token a b -> Parser Token Rhs b
rhs flat eq = rhsExpr <*> localDefs flat
  where rhsExpr =  SimpleRhs <$-> eq <*> position <*> expr flat
               <|> GuardedRhs <$> many1 (condExpr flat eq)

localDefs :: Bool -> Parser Token [Decl] a
localDefs flat = token KW_where <-*> layout (valueDecls flat) `opt` []

externalDecl :: Parser Token Decl a
externalDecl = ExternalDecl
            <$> position <*-> token KW_external
            <*> callConv <*> (Just <$> string `opt` Nothing)
            <*> fun <*-> token DoubleColon <*> type0
  where callConv =  CallConvPrimitive <$-> token Id_primitive
                <|> CallConvCCall <$-> token Id_ccall
                <?> "Unsupported calling convention"

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

type0 :: Parser Token TypeExpr a
type0 = type1 `chainr1` (ArrowType <$-> token RightArrow)

type1 :: Parser Token TypeExpr a
type1 = ConstructorType <$> qtycon <*> many type2
     <|> type2 <\> qtycon

type2 :: Parser Token TypeExpr a
type2 = anonType <|> identType <|> parenType <|> listType

anonType :: Parser Token TypeExpr a
anonType = VariableType <$> anonIdent

identType :: Parser Token TypeExpr a
identType = VariableType <$> tyvar
        <|> flip ConstructorType [] <$> qtycon <\> tyvar

parenType :: Parser Token TypeExpr a
parenType = parens tupleType

tupleType :: Parser Token TypeExpr a
tupleType = type0 <??> (tuple <$> many1 (comma <-*> type0))
      `opt` TupleType []
  where tuple tys ty = TupleType (ty : tys)

listType :: Parser Token TypeExpr a
listType = ListType <$> brackets type0

-- ---------------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------------

literal :: Parser Token Literal a
literal = mk Char   <$> char
      <|> mkInt     <$> integer
      <|> mk Float  <$> float
      <|> mk String <$> string

-- ---------------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------------

constrTerm0 :: Parser Token ConstrTerm a
constrTerm0 = constrTerm1 `chainr1` (flip InfixPattern <$> gconop)

constrTerm1 :: Parser Token ConstrTerm a
constrTerm1 =  varId <**> identPattern'
           <|> ConstructorPattern <$> qConId <\> varId <*> many constrTerm2
           <|> minus <**> negNum
           <|> fminus <**> negFloat
           <|> leftParen <-*> parenPattern'
           <|> constrTerm2 <\> qConId <\> leftParen
  where
    identPattern' =  optAsPattern
                  <|> conPattern <$> many1 constrTerm2
    parenPattern' = minus <**> minusPattern negNum
                  <|> fminus <**> minusPattern negFloat
                  <|> gconPattern
                  <|> funSym <\> minus <\> fminus <*-> rightParen
                                                  <**> identPattern'
                  <|> parenTuplePattern <\> minus <\> fminus <*-> rightParen
    minusPattern p = rightParen <-*> identPattern'
                  <|> parenMinusPattern p <*-> rightParen
    gconPattern = ConstructorPattern <$> gconId <*-> rightParen
                                      <*> many constrTerm2
    conPattern ts = flip ConstructorPattern ts . qualify

constrTerm2 :: Parser Token ConstrTerm a
constrTerm2 =  literalPattern <|> anonPattern <|> identPattern
           <|> parenPattern   <|> listPattern <|> lazyPattern
           <|> recordPattern

literalPattern :: Parser Token ConstrTerm a
literalPattern = LiteralPattern <$> literal

anonPattern :: Parser Token ConstrTerm a
anonPattern = VariablePattern <$> anonIdent

identPattern :: Parser Token ConstrTerm a
identPattern =  varId <**> optAsPattern
            <|> flip ConstructorPattern [] <$> qConId <\> varId

parenPattern :: Parser Token ConstrTerm a
parenPattern = leftParen <-*> parenPattern'
  where parenPattern' = minus <**> minusPattern negNum
                   <|> fminus <**> minusPattern negFloat
                   <|> flip ConstructorPattern [] <$> gconId <*-> rightParen
                   <|> funSym <\> minus <\> fminus <*-> rightParen
                                                   <**> optAsPattern
                   <|> parenTuplePattern <\> minus <\> fminus <*-> rightParen
        minusPattern p = rightParen <-*> optAsPattern
                     <|> parenMinusPattern p <*-> rightParen

listPattern :: Parser Token ConstrTerm a
listPattern = mk' ListPattern <$> brackets (constrTerm0 `sepBy` comma)

lazyPattern :: Parser Token ConstrTerm a
lazyPattern = mk LazyPattern <$-> token Tilde <*> constrTerm2

recordPattern :: Parser Token ConstrTerm a
recordPattern = layoutOff <-*> braces content
  where
  content = RecordPattern <$> fields <*> record
  fields = fieldPatt `sepBy` comma
  fieldPatt = Field <$> position <*> labId <*-> checkEquals <*> constrTerm0
  record = Just <$-> checkBar <*> constrTerm2 `opt` Nothing

-- ---------------------------------------------------------------------------
-- Partial patterns used in the combinators above, but also for parsing
-- the left-hand side of a declaration.
-- ---------------------------------------------------------------------------

gconId :: Parser Token QualIdent a
gconId = colon <|> tupleCommas

negNum,negFloat :: Parser Token (Ident -> ConstrTerm) a
negNum = flip NegativePattern
         <$> (mkInt <$> integer <|> mk Float <$> float)
negFloat = flip NegativePattern . mk Float
           <$> (fromIntegral <$> integer <|> float)

optAsPattern :: Parser Token (Ident -> ConstrTerm) a
optAsPattern = flip AsPattern <$-> token At <*> constrTerm2
         `opt` VariablePattern

optInfixPattern :: Parser Token (ConstrTerm -> ConstrTerm) a
optInfixPattern = infixPat <$> gconop <*> constrTerm0
            `opt` id
  where infixPat op t2 t1 = InfixPattern t1 op t2

optTuplePattern :: Parser Token (ConstrTerm -> ConstrTerm) a
optTuplePattern = tuple <$> many1 (comma <-*> constrTerm0)
            `opt` ParenPattern
  where tuple ts t = mk TuplePattern (t:ts)

parenMinusPattern :: Parser Token (Ident -> ConstrTerm) a
                  -> Parser Token (Ident -> ConstrTerm) a
parenMinusPattern p = p <.> optInfixPattern <.> optTuplePattern

parenTuplePattern :: Parser Token ConstrTerm a
parenTuplePattern = constrTerm0 <**> optTuplePattern
              `opt` mk TuplePattern []

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

condExpr :: Bool -> Parser Token a b -> Parser Token CondExpr b
condExpr flat eq =
  CondExpr <$> position <*-> bar <*> expr0 flat <*-> eq <*> expr flat

expr :: Bool -> Parser Token Expression a
expr flat = expr0 flat <??> (flip Typed <$-> token DoubleColon <*> type0)

expr0 :: Bool -> Parser Token Expression a
expr0 flat = expr1 flat `chainr1` (flip InfixApply <$> infixOp)

expr1 :: Bool -> Parser Token Expression a
expr1 flat = UnaryMinus <$> (minus <|> fminus) <*> expr2 flat
         <|> expr2 flat

expr2 :: Bool -> Parser Token Expression a
expr2 flat = lambdaExpr flat <|> letExpr flat <|> doExpr flat
         <|> ifExpr flat <|> caseExpr flat
         <|> expr3 flat <**> applicOrSelect
  where
  applicOrSelect = flip RecordSelection
                   <$-> (token RightArrow <?> "-> expected")
                    <*> labId
                   <|?> (\es e -> foldl1 Apply (e:es))
                    <$> many (expr3 flat)

expr3 :: Bool -> Parser Token Expression a
expr3 flat =  constant       <|> anonFreeVariable <|> variable
          <|> parenExpr flat <|> listExpr flat    <|> recordExpr flat

constant :: Parser Token Expression a
constant = Literal <$> literal

anonFreeVariable :: Parser Token Expression a
anonFreeVariable =  (\ p v -> Variable $ qualify $ addPositionIdent p v)
                <$> position <*> anonIdent

variable :: Parser Token Expression a
variable = Variable <$> qFunId

parenExpr :: Bool -> Parser Token Expression a
parenExpr flat = parens pExpr
  where pExpr = (minus <|> fminus) <**> minusOrTuple
            <|> Constructor <$> tupleCommas
            <|> leftSectionOrTuple <\> minus <\> fminus
            <|> opOrRightSection <\> minus <\> fminus
          `opt` mk Tuple []
        minusOrTuple = flip UnaryMinus <$> expr1 flat <.> infixOrTuple
                 `opt` Variable . qualify
        leftSectionOrTuple = expr1 flat <**> infixOrTuple
        infixOrTuple = ($ id) <$> infixOrTuple'
        infixOrTuple' = infixOp <**> leftSectionOrExp
                    <|> (.) <$> (optType <.> tupleExpr)
        leftSectionOrExp = expr1 flat <**> (infixApp <$> infixOrTuple')
                     `opt` leftSection
        optType = flip Typed <$-> token DoubleColon <*> type0
            `opt` id
        tupleExpr = tuple <$> many1 (comma <-*> expr flat)
              `opt` Paren
        opOrRightSection = qFunSym <**> optRightSection
                       <|> colon <**> optCRightSection
                       <|> infixOp <\> colon <\> qFunSym <**> rightSection
        optRightSection = (. InfixOp) <$> rightSection `opt` Variable
        optCRightSection = (. InfixConstr) <$> rightSection `opt` Constructor
        rightSection = flip RightSection <$> expr0 flat
        infixApp f e2 op g e1 = f (g . InfixApply e1 op) e2
        leftSection op f e = LeftSection (f e) op
        tuple es e = mk Tuple (e:es)

infixOp :: Parser Token InfixOp a
infixOp = InfixOp <$> qfunop
      <|> InfixConstr <$> colon

listExpr :: Bool -> Parser Token Expression a
listExpr flat = brackets (elements `opt` mk' List [])
  where elements = expr flat <**> rest
        rest = comprehension
           <|> enumeration (flip EnumFromTo) EnumFrom
           <|> comma <-*> expr flat <**>
               (enumeration (flip3 EnumFromThenTo) (flip EnumFromThen)
               <|> list <$> many (comma <-*> expr flat))
         `opt` (\e -> mk' List [e])
        comprehension = flip (mk ListCompr) <$-> bar <*> quals flat
        enumeration enumTo enum =
          token DotDot <-*> (enumTo <$> expr flat `opt` enum)
        list es e2 e1 = mk' List (e1:e2:es)
        flip3 f x y z = f z y x

recordExpr :: Bool -> Parser Token Expression a
recordExpr flat = layoutOff <-*> braces content
  where
    content =    RecordConstr <$> fieldConstr `sepBy` comma
            <|?> RecordUpdate <$> fieldUpdate `sepBy` comma
                              <*-> checkBar <*> expr flat
    fieldConstr = Field <$> position <*> labId
                  <*-> checkEquals <*> expr flat
    fieldUpdate = Field <$> position <*> labId
                  <*-> checkBinds <*> expr flat

lambdaExpr :: Bool -> Parser Token Expression a
lambdaExpr flat =   mk Lambda
               <$-> token Backslash <*> many1 constrTerm2
               <*-> (token RightArrow <?> "-> expected") <*> expr flat

letExpr :: Bool -> Parser Token Expression a
letExpr flat = Let <$-> token KW_let <*> layout (valueDecls flat)
                   <*-> (token KW_in <?> "in expected") <*> expr flat

doExpr :: Bool -> Parser Token Expression a
doExpr flat = uncurry Do <$-> token KW_do <*> layout (stmts flat)

ifExpr :: Bool -> Parser Token Expression a
ifExpr flat =   mk IfThenElse
           <$-> token KW_if <*> expr flat
           <*-> (token KW_then <?> "then expected") <*> expr flat
           <*-> (token KW_else <?> "else expected") <*> expr flat

caseExpr :: Bool -> Parser Token Expression a
caseExpr flat = mk Case <$-> token KW_case <*> expr flat
                <*-> (token KW_of <?> "of expected") <*> layout (alts flat)

alts :: Bool -> Parser Token [Alt] a
alts flat = alt flat `sepBy1` semicolon

alt :: Bool -> Parser Token Alt a
alt flat = Alt <$> position
               <*> constrTerm0
               <*> rhs flat (token RightArrow <?> "-> expected")

-- ---------------------------------------------------------------------------
-- \paragraph{Statements in list comprehensions and \texttt{do} expressions}
-- Parsing statements is a bit difficult because the syntax of patterns
-- and expressions largely overlaps. The parser will first try to
-- recognize the prefix \emph{Pattern}~\texttt{<-} of a binding statement
-- and if this fails fall back into parsing an expression statement. In
-- addition, we have to be prepared that the sequence
-- \texttt{let}~\emph{LocalDefs} can be either a let-statement or the
-- prefix of a let expression.
-- ---------------------------------------------------------------------------

stmts :: Bool -> Parser Token ([Statement],Expression) a
stmts flat = stmt flat (reqStmts flat) (optStmts flat)

reqStmts :: Bool -> Parser Token (Statement -> ([Statement],Expression)) a
reqStmts flat = (\(sts,e) st -> (st : sts,e)) <$-> semicolon <*> stmts flat

optStmts :: Bool -> Parser Token (Expression -> ([Statement],Expression)) a
optStmts flat = succeed (mk StmtExpr) <.> reqStmts flat
          `opt` (,) []

quals :: Bool -> Parser Token [Statement] a
quals flat = stmt flat (succeed id) (succeed $ mk StmtExpr) `sepBy1` comma

stmt :: Bool -> Parser Token (Statement -> a) b
     -> Parser Token (Expression -> a) b -> Parser Token a b
stmt flat stmtCont exprCont = letStmt flat stmtCont exprCont
                          <|> exprOrBindStmt flat stmtCont exprCont

letStmt :: Bool -> Parser Token (Statement -> a) b
        -> Parser Token (Expression -> a) b -> Parser Token a b
letStmt flat stmtCont exprCont =
  token KW_let <-*> layout (valueDecls flat) <**> optExpr
  where optExpr = flip Let <$-> token KW_in <*> expr flat <.> exprCont
              <|> succeed StmtDecl <.> stmtCont

exprOrBindStmt :: Bool -> Parser Token (Statement -> a) b
               -> Parser Token (Expression -> a) b
               -> Parser Token a b
exprOrBindStmt flat stmtCont exprCont =
       mk StmtBind <$> constrTerm0 <*-> leftArrow <*> expr flat <**> stmtCont
  <|?> expr flat <\> token KW_let <**> exprCont

-- ---------------------------------------------------------------------------
-- Literals, identifiers, and (infix) operators
-- ---------------------------------------------------------------------------

char :: Parser Token Char a
char = cval <$> token CharTok

{-
int, checkInt :: Parser Token Int a
int = ival <$> token IntTok
checkInt = int <?> "integer number expected"
-}

float :: Parser Token Double a
float = fval <$> token FloatTok

-- checkFloat :: Parser Token Double a
-- checkFloat = float <?> "floating point number expected"

integer :: Parser Token Integer a
integer = intval <$> token IntegerTok

-- checkInteger :: Parser Token Integer a
-- checkInteger = integer <?> "integer number expected"

string :: Parser Token String a
string = sval <$> token StringTok

tycon, tyvar :: Parser Token Ident a
tycon = conId
tyvar = varId

qtycon :: Parser Token QualIdent a
qtycon = qConId

varId, funId, conId, labId :: Parser Token Ident a
varId = ident
funId = ident
conId = ident
labId = renameLabel <$> ident

funSym, conSym :: Parser Token Ident a
funSym = sym
conSym = sym

var, fun, con :: Parser Token Ident a
var = varId <|> parens (funSym <?> "operator symbol expected")
fun = funId <|> parens (funSym <?> "operator symbol expected")
con = conId <|> parens (conSym <?> "operator symbol expected")

funop, conop :: Parser Token Ident a
funop = funSym <|> backquotes (funId <?> "operator name expected")
conop = conSym <|> backquotes (conId <?> "operator name expected")

qFunId :: Parser Token QualIdent a
qFunId = qIdent

qConId :: Parser Token QualIdent a
qConId = qIdent

-- qLabId :: Parser Token QualIdent a
-- qLabId = qIdent

qFunSym :: Parser Token QualIdent a
qFunSym = qSym

qConSym :: Parser Token QualIdent a
qConSym = qSym

gConSym :: Parser Token QualIdent a
gConSym = qConSym <|> colon

qfun :: Parser Token QualIdent a
qfun = qFunId <|> parens (qFunSym <?> "operator symbol expected")

-- qcon :: Parser Token QualIdent a
-- qcon = qConId <|> parens (qConSym <?> "operator symbol expected")

qfunop, gconop :: Parser Token QualIdent a
qfunop = qFunSym <|> backquotes (qFunId <?> "operator name expected")
gconop = gConSym <|> backquotes (qConId <?> "operator name expected")

-- qconop :: Parser Token QualIdent a
-- qconop = qConSym <|> backquotes (qConId <?> "operator name expected")

anonIdent :: Parser Token Ident a
anonIdent = anonId <$-> token Underscore

mIdent :: Parser Token ModuleIdent a
mIdent = mIdent' <$> position <*>
     tokens [Id,QId,Id_as,Id_ccall,Id_forall,Id_hiding,
             Id_interface,Id_primitive,Id_qualified]
  where mIdent' p a = addPositionModuleIdent p $
                     mkMIdent (modulVal a ++ [sval a])

ident :: Parser Token Ident a
ident = (\ pos -> mkIdentPosition pos . sval) <$> position <*>
       tokens [Id,Id_as,Id_ccall,Id_forall,Id_hiding,
               Id_interface,Id_primitive,Id_qualified]

qIdent :: Parser Token QualIdent a
qIdent =  qualify  <$> ident
      <|> mkQIdent <$> position <*> token QId
  where mkQIdent p a = qualifyWith (mkMIdent (modulVal a))
                                   (mkIdentPosition p (sval a))

sym :: Parser Token Ident a
sym = (\ pos -> mkIdentPosition pos . sval) <$> position <*>
      tokens [Sym, SymDot, SymMinus, SymMinusDot]

qSym :: Parser Token QualIdent a
qSym = qualify <$> sym <|> mkQIdent <$> position <*> token QSym
  where mkQIdent p a = qualifyWith (mkMIdent (modulVal a))
                                   (mkIdentPosition p (sval a))

colon :: Parser Token QualIdent a
colon = (\ p _ -> qualify $ addPositionIdent p consId) <$>
        position <*> token Colon

minus :: Parser Token Ident a
minus = (\ p _ -> addPositionIdent p minusId) <$>
        position <*> token SymMinus

fminus :: Parser Token Ident a
fminus = (\ p _ -> addPositionIdent p fminusId) <$>
        position <*> token SymMinusDot

tupleCommas :: Parser Token QualIdent a
tupleCommas = (\ p -> qualify . addPositionIdent p . tupleId . succ . length )
              <$> position <*> many1 comma

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

layout :: Parser Token a b -> Parser Token a b
layout p =  layoutOff <-*> bracket leftBraceSemicolon p rightBrace
        <|> layoutOn  <-*> p <*-> (token VRightBrace <|> layoutEnd)

-- ---------------------------------------------------------------------------
-- Bracket combinators
-- ---------------------------------------------------------------------------

braces :: Parser Token a b -> Parser Token a b
braces p = bracket leftBrace p rightBrace

brackets :: Parser Token a b -> Parser Token a b
brackets p = bracket leftBracket p rightBracket

parens :: Parser Token a b -> Parser Token a b
parens p = bracket leftParen p rightParen

backquotes :: Parser Token a b -> Parser Token a b
backquotes p = bracket backquote p checkBackquote

-- ---------------------------------------------------------------------------
-- Simple token parsers
-- ---------------------------------------------------------------------------

token :: Category -> Parser Token Attributes a
token c = attr <$> symbol (Token c NoAttributes)
  where attr (Token _ a) = a

tokens :: [Category] -> Parser Token Attributes a
tokens = foldr1 (<|>) . map token

tokenOps :: [(Category,a)] -> Parser Token a b
tokenOps cs = ops [(Token c NoAttributes, x) | (c,x) <- cs]

comma, semicolon, bar, equals, binds :: Parser Token Attributes a
comma = token Comma
semicolon = token Semicolon <|> token VSemicolon
bar = token Bar
equals = token Equals
binds = token Binds

-- dot :: Parser Token Attributes a
-- dot = token Sym_Dot

checkBar, checkEquals, checkBinds :: Parser Token Attributes a
checkBar = bar <?> "| expected"
checkEquals = equals <?> "= expected"
checkBinds = binds <?> ":= expected"

backquote, checkBackquote :: Parser Token Attributes a
backquote = token Backquote
checkBackquote = backquote <?> "backquote (`) expected"

leftParen, rightParen :: Parser Token Attributes a
leftParen = token LeftParen
rightParen = token RightParen

leftBracket, rightBracket :: Parser Token Attributes a
leftBracket = token LeftBracket
rightBracket = token RightBracket

leftBrace, leftBraceSemicolon, rightBrace :: Parser Token Attributes a
leftBrace = token LeftBrace
leftBraceSemicolon = token LeftBraceSemicolon
rightBrace = token RightBrace

leftArrow :: Parser Token Attributes a
leftArrow = token LeftArrow

-- ---------------------------------------------------------------------------
-- Ident
-- ---------------------------------------------------------------------------

mkIdentPosition :: Position -> String -> Ident
mkIdentPosition pos = addPositionIdent pos . mkIdent
