{- |
    Module      :  $Header$
    Description :  A Parser for Curry
    Copyright   :  (c) 1999 - 2004 Wolfgang Lux
                       2005        Martin Engelke
                       2011 - 2013 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    The Curry parser is implemented using the (mostly) LL(1) parsing
    combinators implemented in 'Curry.Base.LLParseComb'.
-}

module Curry.Syntax.Parser
  ( parseSource, parseHeader, parseInterface, parseGoal
  ) where

import Curry.Base.Ident
import Curry.Base.Position    (Position, mk, mk')
import Curry.Base.Message     (MessageM)
import Curry.Base.LLParseComb

import Curry.Syntax.Lexer (Token (..), Category (..), Attributes (..), lexer)
import Curry.Syntax.Type
import Curry.Syntax.Utils (mkInt, addSrcRefs)

import Prelude hiding (exp)

import Data.Maybe
import Data.List

-- |Parse a 'Module'
parseSource :: FilePath -> String -> MessageM Module
parseSource path = fmap addSrcRefs
                 . fullParser (moduleHeader <*> layout topDecls) lexer path

-- |Parse a 'Module' header
parseHeader :: FilePath -> String -> MessageM Module
parseHeader = prefixParser (moduleHeader <*> succeed []) lexer

-- |Parse an 'Interface'
parseInterface :: FilePath -> String -> MessageM [Interface]
parseInterface = fullParser interfaces lexer

-- |Parse a 'Goal'
parseGoal :: String -> MessageM Goal
parseGoal = fullParser goal lexer ""

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

-- |Parser for a module header
moduleHeader :: Parser Token ([Decl] -> Module) a
moduleHeader =  Module <$-> token KW_module <*>  modIdent
                       <*>  optionMaybe exportSpec
                       <*-> checkWhere
                       <*>  importDecls
            <|> Module mainMIdent Nothing <$> importDecls

-- |Parser for an export specification
exportSpec :: Parser Token ExportSpec a
exportSpec = Exporting <$> position <*> parens (export `sepBy` comma)

-- |Parser for an export item
export :: Parser Token Export a
export =  qtycon <**> (parens spec `opt` Export)         -- type constructor
      <|> Export <$> qfun <\> qtycon                     -- fun
      <|> ExportModule <$-> token KW_module <*> modIdent -- module
  where spec =       ExportTypeAll  <$-> token DotDot
            <|> flip ExportTypeWith <$>  con `sepBy` comma

-- |Parser for import declarations
importDecls :: Parser Token [ImportDecl] a
importDecls = optional leftBrace <-*> many (importDecl <*-> many semicolon)

-- |Parser for a single import declaration
importDecl :: Parser Token ImportDecl a
importDecl =  flip . ImportDecl
          <$> tokenPos KW_import
          <*> flag (token Id_qualified)
          <*> modIdent
          <*> optionMaybe (token Id_as <-*> modIdent)
          <*> optionMaybe importSpec

-- |Parser for an import specification
importSpec :: Parser Token ImportSpec a
importSpec =   position
          <**> (Hiding <$-> token Id_hiding `opt` Importing)
          <*>  parens (spec `sepBy` comma)
  where
  spec    =  tycon <**> (parens constrs `opt` Import)
         <|> Import <$> fun <\> tycon
  constrs =  ImportTypeAll       <$-> token DotDot
         <|> flip ImportTypeWith <$>  con `sepBy` comma

-- ---------------------------------------------------------------------------
-- Interfaces
-- ---------------------------------------------------------------------------

-- |Parser for all interfaces (type class specific as well as non type class
-- specific)
interfaces :: Parser Token [Interface] a
interfaces = (\i1 i2 -> i1 : [i2]) <$> 
  interface Id_interface <*> interface Id_interfaceTypeClasses

-- |Parser for an interface
interface :: Category -> Parser Token Interface a
interface tk = Interface
         <$-> token tk           <*>  modIdent
         <*-> checkWhere         <*-> leftBrace
         <*>  iImportDecls
         <*>  intfDecls          <*-> rightBrace

-- |Parser for interface import declarations
iImportDecls :: Parser Token [IImportDecl] a
iImportDecls = iImportDecl `sepBy` semicolon

-- |Parser for a single interface import declaration
iImportDecl :: Parser Token IImportDecl a
iImportDecl = IImportDecl <$> tokenPos KW_import <*> modIdent

-- |Parser for interface declarations
intfDecls :: Parser Token [IDecl] a
intfDecls = intfDecl `sepBy` semicolon

-- |Parser for a single interface declaration
intfDecl :: Parser Token IDecl a
intfDecl = choice [ iInfixDecl, iHidingDecl, iDataDecl, iNewtypeDecl
                  , iTypeDecl , iFunctionDecl <\> token Id_hiding
                  , iClassDecl, iInstanceDecl ]

-- |Parser for an interface infix declaration
iInfixDecl :: Parser Token IDecl a
iInfixDecl = infixDeclLhs IInfixDecl <*> qfunop

-- |Parser for an interface hiding declaration
iHidingDecl :: Parser Token IDecl a
iHidingDecl = tokenPos Id_hiding <**> (hDataDecl <|> hClassDecl <|> hFuncDecl)
  where
  hDataDecl = hiddenData <$-> token KW_data <*> qtycon <*> many tyvar
  hClassDecl = const <$> iHidingClassDecl
  hFuncDecl = hidingFunc <$> arity <*-> token DoubleColon <*> 
    (Context <$> (parens $ (contextElem `sepBy` comma))) <*-> token DoubleArrow
     <*> type0 True
  hiddenData tc tvs p = HidingDataDecl p tc tvs
  -- TODO: 0 was inserted to type check, but what is the meaning of this field?
  hidingFunc a cx ty p = IFunctionDecl p (qualify (mkIdent "hiding")) a cx ty

-- |Parser for an interface data declaration
iDataDecl :: Parser Token IDecl a
iDataDecl = iTypeDeclLhs IDataDecl KW_data <*> constrs
  where constrs = equals <-*> iConstrDecl `sepBy1` bar `opt` []
        iConstrDecl =  Just    <$>  constrDecl <\> token Underscore
                   <|> Nothing <$-> token Underscore

-- |Parser for an interface newtype declaration
iNewtypeDecl :: Parser Token IDecl a
iNewtypeDecl = iTypeDeclLhs INewtypeDecl KW_newtype
               <*-> equals <*> newConstrDecl

-- |Parser for an interface type synonym declaration
iTypeDecl :: Parser Token IDecl a
iTypeDecl = iTypeDeclLhs ITypeDecl KW_type
            <*-> equals <*> type0 True

-- |Parser for an interface function declaration
iFunctionDecl :: Parser Token IDecl a
iFunctionDecl =  IFunctionDecl <$> position <*> qIfun <*> arity
            <*-> token DoubleColon 
            <*> (Context <$> (parens $ (contextElem `sepBy` comma)))
            <*-> token DoubleArrow
            <*> type0 True

-- | the separator used for constructed function and type names
separator :: Parser Token Attributes a
separator = token Colon <*-> token Underscore

-- |Generated functions like the selection functions and dictionaries
-- use the separator ":" for seperating elements from each other, and
-- for assuring that the method/type names will not overlap with names
-- used in the source code. 
-- As we write the name as-is into the interface file, we have to consider, 
-- that function/type names when they are parsed consist of multiple QualIdents, 
-- seperated by colons.  
qIfun :: Parser Token QualIdent a
qIfun = composedIdent <|> parens (qFunSym <?> "operator symbol expected")

-- |an identifier that is composed of qualified identifiers (either identifiers
-- or operators) seperated by the separator tokens
composedIdent :: Parser Token QualIdent a
composedIdent = mkQIdent <$> (qIdent <|> qSym) `sepBy1` separator

-- |converts a list of QualIdents to one QualIdent: 
-- @M1.q1 M2.q2 ... Mn.qn@ is converted to @M1."q1:M2.q2:...:Mn.qn"@
-- and
-- @q1 M2.q2 ... Mn.qn@ to @"q1:M2.q2:...:Mn.qn"@
mkQIdent :: [QualIdent] -> QualIdent
-- the first qualident determines the module
mkQIdent (qid:qids) = let mdl = qidModule qid in 
  (if isJust mdl then qualifyWith (fromJust mdl) else qualify) $ 
  mkIdent (intercalate sep $ show (qidIdent qid) : map show qids)
mkQIdent [] = error "qIfun"

-- |Parser for function's arity
arity :: Parser Token Int a
arity = int `opt` 0

iTypeDeclLhs :: (Position -> QualIdent -> [Ident] -> a) -> Category
             -> Parser Token a b
iTypeDeclLhs f kw = f <$> tokenPos kw <*> 
  (mkQIdent <$> qIdent `sepBy1` separator) <*> many tyvar

-- ----------------------------------------------------------------------------
-- type classes specific interface parsers
-- ----------------------------------------------------------------------------

-- |Parser for a interface class declaration of the following form:
-- @
-- class [K, K2] L a where {
-- funL 1 :: a -> a
-- }
-- @
iClassDecl :: Parser Token IDecl a
iClassDecl = IClassDecl <$> tokenPos KW_class <*> brackets (qtycls `sepBy` comma) 
  <*> qtycls <*> tyvar <*-> checkWhere <*-> leftBrace 
  <*> classTySigs <*-> rightBrace  <*> brackets (fun `sepBy` comma)
  <*> brackets (composedIdent `sepBy` comma)


iHidingClassDecl :: Parser Token IDecl a
iHidingClassDecl = IHidingClassDecl <$> tokenPos KW_class <*> brackets (qtycls `sepBy` comma) 
  <*> qtycls <*> tyvar <*-> checkWhere <*-> leftBrace 
  <*> hClassTySigs <*-> rightBrace <*> brackets (fun `sepBy` comma)

-- |Parser that parses all type signatures of a hidden class declaration
hClassTySigs :: Parser Token [IDecl] a
hClassTySigs = hClassTySig `sepBy` semicolon

-- |Parser that parses one type signature of a hidden class declaration
hClassTySig :: Parser Token IDecl a
hClassTySig = iFunctionDecl

-- |Parses all type signatures of a public class declaration  
classTySigs :: Parser Token [(Bool, IDecl)] a
classTySigs = classTySig `sepBy` semicolon

-- |Parses one type signature of a public class declaration
classTySig :: Parser Token (Bool, IDecl) a
classTySig = (\public f -> (public, f)) <$> 
  (False <$-> token Id_hiding <|> True <$-> token Id_public) <*>
  iFunctionDecl

-- |Parses an interface declaration of the following form:
-- @
-- instance (C_1 a_i1, ..., C_n a_in) => C (T a_1 ... a_k)
-- @
-- where T is a type constructor (can also be a special type constructor (list, 
-- unit, tuple, arrow)) 
iInstanceDecl :: Parser Token IDecl a
iInstanceDecl =
  IInstanceDecl <$> tokenPos KW_instance <*> brackets (optionMaybe mIdent) 
  <*> parens (sepBy simpleclass comma)
  <*-> token DoubleArrow <*> qtycls 
  <*-> leftParen <*> gtycon <*> many tyvar 
  <*-> rightParen <*> brackets (composedIdent `sepBy` comma)

-- ---------------------------------------------------------------------------
-- Top-Level Declarations
-- ---------------------------------------------------------------------------

topDecls :: Parser Token [Decl] a
topDecls = topDecl `sepBy` semicolon

topDecl :: Parser Token Decl a
topDecl = choice [ dataDecl, newtypeDecl, typeDecl, foreignDecl
                 , infixDecl, functionDecl, classDecl, instanceDecl ]

infixDecl :: Parser Token Decl a
infixDecl = infixDeclLhs InfixDecl <*> funop `sepBy1` comma

infixDeclLhs :: (Position -> Infix -> Integer -> a) -> Parser Token a b
infixDeclLhs f = f <$> position <*> tokenOps infixKW
                   <*> (integer <?> "precedence level expected") -- TODO: change to int?
  where
  infixKW = [(KW_infix, Infix), (KW_infixl, InfixL), (KW_infixr, InfixR)]

dataDecl :: Parser Token Decl a
dataDecl = typeDeclLhs DataDecl KW_data <*> constrs
  where constrs = equals <-*> constrDecl `sepBy1` bar `opt` []

newtypeDecl :: Parser Token Decl a
newtypeDecl = typeDeclLhs NewtypeDecl KW_newtype <*-> equals <*> newConstrDecl

typeDecl :: Parser Token Decl a
typeDecl = typeDeclLhs TypeDecl KW_type <*-> equals <*> type0 True

typeDeclLhs :: (Position -> Ident -> [Ident] -> a) -> Category
            -> Parser Token a b
typeDeclLhs f kw = f <$> tokenPos kw <*> tycon <*> many anonOrTyvar

constrDecl :: Parser Token ConstrDecl a
constrDecl = position <**> (existVars <**> constr)
  where
  constr =  conId     <**> identDecl
        <|> leftParen <-*> parenDecl
        <|> type1 False <\> conId <\> leftParen <**> opDecl
  identDecl = many (type2 False) <**> (conType <$> opDecl `opt` conDecl)
  parenDecl =  conOpDeclPrefix
           <$> conSym    <*-> rightParen <*> type2 False <*> type2 False
           <|> tupleType False <*-> rightParen <**> opDecl
  opDecl = conOpDecl <$> conop <*> type1 False
  conType f tys c                  = f $ ConstructorType (qualify c) tys
  conDecl tys c tvs p              = ConstrDecl p tvs c tys
  conOpDecl op ty2 ty1 tvs p       = ConOpDecl p tvs ty1 op ty2
  conOpDeclPrefix op ty1 ty2 tvs p = ConOpDecl p tvs ty1 op ty2

newConstrDecl :: Parser Token NewConstrDecl a
newConstrDecl = NewConstrDecl <$> position <*> existVars <*> con <*> type2 False

valueDecls :: Parser Token [Decl] a
valueDecls  = choice [infixDecl, valueDecl, foreignDecl] `sepBy` semicolon

existVars :: Parser Token [Ident] a
{-
existVars flat
  | flat = succeed []
  | otherwise = token Id_forall <-*> many1 tyvar <*-> dot `opt` []
-}
existVars = succeed []

functionDecl :: Parser Token Decl a
functionDecl = position <**> decl
  where
  decl = fun `sepBy1` comma <**> funListDecl
    <|?> funRule

funRule :: Parser Token (Position -> Decl) a
funRule = funDecl <$> lhs <*> declRhs
  where
  lhs = (\f -> (f, FunLhs f [])) <$> fun <|?> funLhs

valueDecl :: Parser Token Decl a
valueDecl = position <**> decl
  where
  decl =   var `sepBy1` comma <**> valListDecl
      <|?> valDecl <$> pattern0 <*> declRhs
      <|?> funDecl <$> curriedLhs <*> declRhs

  valDecl (ConstructorPattern c ts)
    | not (isConstrId c) = funDecl (f, FunLhs f ts)
    where f = unqualify c
  valDecl t = opDecl id t

  opDecl f (InfixPattern t1 op t2)
    | isConstrId op = opDecl (f . InfixPattern t1 op) t2
    | otherwise    = funDecl (op', OpLhs (f t1) op' t2)
    where op' = unqualify op
  opDecl f t = patDecl (f t)

  isConstrId c = c == qConsId || isQualified c || isQTupleId c

funDecl :: (Ident,Lhs) -> Rhs -> Position -> Decl
funDecl (f,lhs) rhs' p = FunctionDecl p Nothing (-1) f [Equation p lhs rhs']

patDecl :: Pattern -> Rhs -> Position -> Decl
patDecl t rhs' p = PatternDecl p Nothing (-1) t rhs'

funListDecl :: Parser Token ([Ident] -> Position -> Decl) a
funListDecl =  typeSignature
           <|> externalDecl
  
typeSignature :: Parser Token ([Ident] -> Position -> Decl) a
typeSignature = (typeSig <$-> token DoubleColon <*> context <*-> token DoubleArrow <*> type0 False)
  <|?> (typeSig <$-> token DoubleColon <*> succeed emptyContext <*> type0 False)
  where typeSig cx ty vs p = TypeSig p vs cx ty

externalDecl :: Parser Token ([Ident] -> Position -> Decl) a
externalDecl = flip ExternalDecl <$-> token KW_external 

valListDecl :: Parser Token ([Ident] -> Position -> Decl) a
valListDecl = funListDecl <|> flip FreeDecl <$-> token KW_free

funLhs :: Parser Token (Ident,Lhs) a
funLhs = funLhs'     <$> fun      <*> many1 pattern2
    <|?> flip ($ id) <$> pattern1 <*> opLhs'
    <|?> curriedLhs
  where
  opLhs' = opLhs <$> funSym <*> pattern0
      <|> infixPat <$> gConSym <\> funSym <*> pattern1 <*> opLhs'
      <|> backquote <-*> opIdLhs
  opIdLhs = opLhs <$> funId <*-> checkBackquote <*> pattern0
        <|> infixPat <$> qConId <\> funId <*-> backquote <*> pattern1
                      <*> opLhs'
  funLhs' f ts = (f,FunLhs f ts)
  opLhs op t2 f t1 = (op,OpLhs (f t1) op t2)
  infixPat op t2 f g t1 = f (g . InfixPattern t1 op) t2

curriedLhs :: Parser Token (Ident,Lhs) a
curriedLhs = apLhs <$> parens funLhs <*> many1 pattern2
  where apLhs (f,lhs) ts = (f,ApLhs lhs ts)

declRhs :: Parser Token Rhs a
declRhs = rhs equals

rhs :: Parser Token a b -> Parser Token Rhs b
rhs eq = rhsExpr <*> localDecls
  where rhsExpr =  SimpleRhs  <$-> eq <*> position <*> expr
               <|> GuardedRhs <$>  many1 (condExpr eq)

localDecls :: Parser Token [Decl] a
localDecls = token KW_where <-*> layout valueDecls `opt` []

foreignDecl :: Parser Token Decl a
foreignDecl = ForeignDecl
          <$> tokenPos KW_foreign
          <*> callConv <*> (optionMaybe string)
          <*> fun <*-> token DoubleColon <*> type0 False
  where callConv =  CallConvPrimitive <$-> token Id_primitive
                <|> CallConvCCall     <$-> token Id_ccall
                <?> "Unsupported calling convention"

-- Since the grammer used is not LL(1), we have to use the <|?>
-- operator. The problem is the following:
-- class Eq a
--       ^
-- class Eq a => Ord b
--       ^
-- At the points marked, we cannot decide, whether we have 
-- a context or not, so we don't know whether to apply 
-- "scontext <*-> token DoubleArrow" or "succeed emptyscontext" and "tycls". 
-- What was the right interpretation can only be decided later, 
-- and that's what the <|?> operator does: It tries both parsers 
-- and afterward chooses the appropriate one.
classDecl :: Parser Token Decl a
classDecl = (\p (scon,c,v) decls -> ClassDecl p scon c v decls) <$>
  tokenPos KW_class
  <*> 
    (((,,) <$> scontext <*-> token DoubleArrow
    <*> tycls
    <*> tyvar)
    <|?>
    ((,,) <$> succeed emptyscontext
    <*> tycls
    <*> tyvar))
  <*> (token KW_where <-*> layout classDecls `opt` [])
  where classDecls = oneClassDecl `sepBy` semicolon
        fullTypeSig = position <**> (fun `sepBy1` comma <**> typeSignature)
        -- TODO: support infix declarations
        oneClassDecl = foldl1 (<|?>) [ fullTypeSig, position <**> funRule {-, infixDecl -}]

instanceDecl :: Parser Token Decl a
instanceDecl = 
  (\p (scon, qc, (itycon, vars)) decls 
    -> InstanceDecl p scon qc itycon vars decls)
  <$>
  tokenPos KW_instance
  <*> 
    (((,,) <$> scontext <*-> token DoubleArrow
    <*> qtycls
    <*> inst)
    <|?>
    ((,,) <$> succeed emptyscontext
    <*> qtycls
    <*> inst))
  <*> (token KW_where <-*> layout instDecls `opt` [])
  where instDecls = oneInstDecl `sepBy` semicolon
        oneInstDecl = position <**> funRule

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- type0 ::= type1 ['->' type0]
type0 :: Bool -> Parser Token TypeExpr a
type0 withRecordType = type1 withRecordType `chainr1`
                       (ArrowType <$-> token RightArrow)

-- type1 ::= QTyCon { type2 } | type2
type1 :: Bool -> Parser Token TypeExpr a
type1 withRecordType = ConstructorType <$> qtycon <*> many type2'
    <|> type2' <\> qtycon
  where type2' = type2 withRecordType

-- type2 ::= anonType | identType | parenType | listType | [] | (->) | (,{,})
-- allow also "[]", "(->)" and "(,{,})"!
type2 :: Bool -> Parser Token TypeExpr a
type2 withRecordType
  | withRecordType = (anonType <|> identType <|> parenType True
                  <|> listType True <|> recordType) <|?> gtycon'
  | otherwise      = (anonType <|> identType <|> parenType False
                  <|> listType False) <|?> gtycon'

-- anonType ::= '_'
anonType :: Parser Token TypeExpr a
anonType = VariableType <$> anonIdent

-- identType ::= <identifier>
identType :: Parser Token TypeExpr a
identType = VariableType <$> tyvar
        <|> flip ConstructorType [] <$> qtycon <\> tyvar

-- parenType ::= '(' tupleType ')'
parenType :: Bool -> Parser Token TypeExpr a
parenType withRecordType = parens (tupleType withRecordType)

-- tupleType ::= type0                         (parenthesized type)
--            |  type0 ',' type0 { ',' type0 } (tuple type)
--            |                                (unit type)
tupleType :: Bool -> Parser Token TypeExpr a
tupleType withRecordType = type0 withRecordType <??>
                           (tuple <$> many1 (comma <-*> type0 withRecordType))
                           `opt` TupleType []
  where tuple tys ty = TupleType (ty : tys)

-- listType ::= '[' type0 ']'
listType :: Bool -> Parser Token TypeExpr a
listType withRecordType = ListType <$> brackets (type0 withRecordType)

-- listType ::= '{' labelDecls '}'
recordType :: Parser Token TypeExpr a
recordType =  flip RecordType Nothing
          <$> (layoutOff <-*> braces (labelDecls `sepBy` comma))

-- labelDecls ::= labId '::' type0 [',' labelDecls]
labelDecls :: Parser Token ([Ident], TypeExpr) a
labelDecls = (,) <$> labId `sepBy1` comma <*-> token DoubleColon <*> type0 True

-- ---------------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------------

-- literal ::= '\'' <escaped character> '\''
--          |  <integer>
--          |  '"' <escaped string> '"'
literal :: Parser Token Literal a
literal = mk Char   <$> char
      <|> mkInt     <$> integer
      <|> mk Float  <$> float
      <|> mk String <$> string

-- ---------------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------------

-- pattern0 ::= pattern1 [ ConOp pattern0 ]
pattern0 :: Parser Token Pattern a
pattern0 = pattern1 `chainr1` (flip InfixPattern <$> gconop)

-- pattern1 ::= varId
--           |  QConId { pattern2 }
--           |  '-'  negnum
--           |  '-.' negFloat
--           |  '(' parenPattern'
--           | pattern2
pattern1 :: Parser Token Pattern a
pattern1 =  varId <**> identPattern'
        <|> ConstructorPattern <$> qConId <\> varId <*> many pattern2
        <|> minus <**> negNum
        <|> fminus <**> negFloat
        <|> leftParen <-*> parenPattern'
        <|> pattern2 <\> qConId <\> leftParen
  where
  identPattern' =  optAsPattern
               <|> conPattern <$> many1 pattern2
  parenPattern' =  minus <**> minusPattern negNum
               <|> fminus <**> minusPattern negFloat
               <|> gconPattern
               <|> funSym <\> minus <\> fminus <*-> rightParen
                                               <**> identPattern'
               <|> parenTuplePattern <\> minus <\> fminus <*-> rightParen
  minusPattern p = rightParen <-*> identPattern'
                <|> parenMinusPattern p <*-> rightParen
  gconPattern = ConstructorPattern <$> gconId <*-> rightParen
                                    <*> many pattern2
  conPattern ts = flip ConstructorPattern ts . qualify

pattern2 :: Parser Token Pattern a
pattern2 =  literalPattern <|> anonPattern <|> identPattern
        <|> parenPattern   <|> listPattern <|> lazyPattern
        <|> recordPattern

literalPattern :: Parser Token Pattern a
literalPattern = LiteralPattern <$> literal

anonPattern :: Parser Token Pattern a
anonPattern = VariablePattern <$> anonIdent

identPattern :: Parser Token Pattern a
identPattern =  varId <**> optAsPattern
            <|> flip ConstructorPattern [] <$> qConId <\> varId

parenPattern :: Parser Token Pattern a
parenPattern = leftParen <-*> parenPattern'
  where
  parenPattern' = minus <**> minusPattern negNum
              <|> fminus <**> minusPattern negFloat
              <|> flip ConstructorPattern [] <$> gconId <*-> rightParen
              <|> funSym <\> minus <\> fminus <*-> rightParen
                                              <**> optAsPattern
              <|> parenTuplePattern <\> minus <\> fminus <*-> rightParen
  minusPattern p = rightParen <-*> optAsPattern
                <|> parenMinusPattern p <*-> rightParen

listPattern :: Parser Token Pattern a
listPattern = mk' ListPattern <$> brackets (pattern0 `sepBy` comma)

lazyPattern :: Parser Token Pattern a
lazyPattern = mk LazyPattern <$-> token Tilde <*> pattern2

recordPattern :: Parser Token Pattern a
recordPattern = layoutOff <-*> braces content
  where
  content   = RecordPattern <$> (fieldPatt `sepBy` comma) <*> record
  fieldPatt = Field <$> position <*> labId <*-> checkEquals <*> pattern0
  record    = optionMaybe (checkBar <-*> pattern2)

-- ---------------------------------------------------------------------------
-- Partial patterns used in the combinators above, but also for parsing
-- the left-hand side of a declaration.
-- ---------------------------------------------------------------------------

gconId :: Parser Token QualIdent a
gconId = colon <|> tupleCommas

negNum :: Parser Token (Ident -> Pattern) a
negNum = flip NegativePattern <$> (mkInt <$> integer <|> mk Float <$> float)

negFloat :: Parser Token (Ident -> Pattern) a
negFloat = flip NegativePattern . mk Float
           <$> (fromIntegral <$> integer <|> float)

optAsPattern :: Parser Token (Ident -> Pattern) a
optAsPattern = flip AsPattern <$-> token At <*> pattern2
         `opt` VariablePattern

optInfixPattern :: Parser Token (Pattern -> Pattern) a
optInfixPattern = infixPat <$> gconop <*> pattern0
            `opt` id
  where infixPat op t2 t1 = InfixPattern t1 op t2

optTuplePattern :: Parser Token (Pattern -> Pattern) a
optTuplePattern = tuple <$> many1 (comma <-*> pattern0)
            `opt` ParenPattern
  where tuple ts t = mk TuplePattern (t:ts)

parenMinusPattern :: Parser Token (Ident -> Pattern) a
                  -> Parser Token (Ident -> Pattern) a
parenMinusPattern p = p <.> optInfixPattern <.> optTuplePattern

parenTuplePattern :: Parser Token Pattern a
parenTuplePattern = pattern0 <**> optTuplePattern
              `opt` mk TuplePattern []

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- condExpr ::= '|' expr0 eq expr
condExpr :: Parser Token a b -> Parser Token CondExpr b
condExpr eq = CondExpr <$> position <*-> bar <*> expr0 <*-> eq <*> expr

-- expr ::= expr0 [ '::' [context =>] type0 ]
expr :: Parser Token Expression a
expr = (expr0 <??> 
   (typed <$-> token DoubleColon <*> succeed emptyContext <*> type0 False
    <|?> 
    typed <$-> token DoubleColon <*> context <*-> token DoubleArrow <*> type0 False))
 where typed cons ty exp = Typed Nothing exp cons ty 

-- expr0 ::= expr1 { infixOp expr1 }
expr0 :: Parser Token Expression a
expr0 = expr1 `chainr1` (flip InfixApply <$> infixOp)

-- expr1 ::= - expr2 | -. expr2 | expr2
expr1 :: Parser Token Expression a
expr1 =  UnaryMinus <$> (minus <|> fminus) <*> expr2
     <|> expr2

-- expr2 ::= lambdaExpr | letExpr | doExpr | ifExpr | caseExpr | expr3
expr2 :: Parser Token Expression a
expr2 = choice [ lambdaExpr, letExpr, doExpr, ifExpr, caseExpr
               , expr3 <**> (recordSelect <|?> application) ]
  where
  recordSelect = (flip (foldl RecordSelection))
              <$> many1 (checkSelect <-*> labId)
  application  = (\es e -> foldl1 Apply (e:es)) <$> many expr3

expr3 :: Parser Token Expression a
expr3 = choice
  [constant, anonFreeVariable, variable, parenExpr, listExpr, recordExpr]

constant :: Parser Token Expression a
constant = Literal <$> literal

anonFreeVariable :: Parser Token Expression a
anonFreeVariable =  (\ p v -> Variable Nothing $ qualify $ addPositionIdent p v)
                <$> position <*> anonIdent

variable :: Parser Token Expression a
variable = Variable Nothing <$> qFunId

parenExpr :: Parser Token Expression a
parenExpr = parens pExpr
  where
  pExpr = (minus <|> fminus) <**> minusOrTuple
      <|> Constructor <$> tupleCommas
      <|> leftSectionOrTuple <\> minus <\> fminus
      <|> opOrRightSection <\> minus <\> fminus
      `opt` mk Tuple []
  minusOrTuple = flip UnaryMinus <$> expr1 <.> infixOrTuple
            `opt` Variable Nothing . qualify
  leftSectionOrTuple = expr1 <**> infixOrTuple
  infixOrTuple = ($ id) <$> infixOrTuple'
  infixOrTuple' = infixOp <**> leftSectionOrExp
              <|> (.) <$> (optType <.> tupleExpr)
  leftSectionOrExp = expr1 <**> (infixApp <$> infixOrTuple')
                `opt` leftSection
  optType   = ((typed <$-> token DoubleColon <*> succeed emptyContext <*> type0 False)
          <|?> (typed <$-> token DoubleColon <*> context <*-> 
                           token DoubleArrow <*> type0 False)) `opt` id
  typed cons ty exp = Typed Nothing exp cons ty
  tupleExpr = tuple <$> many1 (comma <-*> expr) `opt` Paren
  opOrRightSection =  qFunSym <**> optRightSection
                  <|> colon   <**> optCRightSection
                  <|> infixOp <\> colon <\> qFunSym <**> rightSection
  optRightSection  = (. InfixOp Nothing) <$> rightSection `opt` Variable Nothing
  optCRightSection = (. InfixConstr    ) <$> rightSection `opt` Constructor
  rightSection     = flip RightSection <$> expr0
  infixApp f e2 op g e1 = f (g . InfixApply e1 op) e2
  leftSection op f e = LeftSection (f e) op
  tuple es e = mk Tuple (e:es)

infixOp :: Parser Token InfixOp a
infixOp = InfixOp Nothing <$> qfunop <|> InfixConstr <$> colon

listExpr :: Parser Token Expression a
listExpr = brackets (elements `opt` mk' List [])
  where
  elements = expr <**> rest
  rest = comprehension
      <|> enumeration (flip EnumFromTo) EnumFrom
      <|> comma <-*> expr <**>
          (enumeration (flip3 EnumFromThenTo) (flip EnumFromThen)
          <|> list <$> many (comma <-*> expr))
    `opt` (\e -> mk' List [e])
  comprehension = flip (mk ListCompr) <$-> bar <*> quals
  enumeration enumTo enum =
    token DotDot <-*> (enumTo <$> expr `opt` enum)
  list es e2 e1 = mk' List (e1:e2:es)
  flip3 f x y z = f z y x

recordExpr :: Parser Token Expression a
recordExpr = layoutOff <-*> braces content
  where
  content        = fieldAccess `sepBy` comma <**> constrOpUpdate
  fieldAccess    = Field <$> position <*> labId <*-> checkBind <*> expr
  constrOpUpdate = flip RecordUpdate <$-> checkBar <*> expr
                   `opt` RecordConstr

lambdaExpr :: Parser Token Expression a
lambdaExpr = mk Lambda <$-> token Backslash <*> many1 pattern2
                       <*-> checkRightArrow <*> expr

letExpr :: Parser Token Expression a
letExpr = Let <$-> token KW_let <*> layout valueDecls
              <*-> (token KW_in <?> "in expected") <*> expr

doExpr :: Parser Token Expression a
doExpr = uncurry Do <$-> token KW_do <*> layout stmts

ifExpr :: Parser Token Expression a
ifExpr = mk IfThenElse
    <$-> token KW_if                         <*> expr
    <*-> (token KW_then <?> "then expected") <*> expr
    <*-> (token KW_else <?> "else expected") <*> expr

caseExpr :: Parser Token Expression a
caseExpr = keyword <*> expr
      <*-> (token KW_of <?> "of expected") <*> layout (alt `sepBy1` semicolon)
  where keyword =  mk Case Flex  <$-> token KW_fcase
               <|> mk Case Rigid <$-> token KW_case

alt :: Parser Token Alt a
alt = Alt <$> position <*> pattern0 <*> rhs checkRightArrow

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

stmts :: Parser Token ([Statement], Expression) a
stmts = stmt reqStmts optStmts

reqStmts :: Parser Token (Statement -> ([Statement], Expression)) a
reqStmts = (\ (sts, e) st -> (st : sts, e)) <$-> semicolon <*> stmts

optStmts :: Parser Token (Expression -> ([Statement],Expression)) a
optStmts = succeed (mk StmtExpr) <.> reqStmts `opt` (,) []

quals :: Parser Token [Statement] a
quals = stmt (succeed id) (succeed $ mk StmtExpr) `sepBy1` comma

stmt :: Parser Token (Statement -> a) b
     -> Parser Token (Expression -> a) b -> Parser Token a b
stmt stmtCont exprCont =  letStmt stmtCont exprCont
                      <|> exprOrBindStmt stmtCont exprCont

letStmt :: Parser Token (Statement -> a) b
        -> Parser Token (Expression -> a) b -> Parser Token a b
letStmt stmtCont exprCont = token KW_let <-*> layout valueDecls <**> optExpr
  where optExpr =  flip Let <$-> token KW_in <*> expr <.> exprCont
               <|> succeed StmtDecl <.> stmtCont

exprOrBindStmt :: Parser Token (Statement -> a) b
               -> Parser Token (Expression -> a) b
               -> Parser Token a b
exprOrBindStmt stmtCont exprCont =
       mk StmtBind <$> pattern0 <*-> leftArrow <*> expr <**> stmtCont
  <|?> expr <\> token KW_let <**> exprCont

-- ---------------------------------------------------------------------------
-- Goals
-- ---------------------------------------------------------------------------

goal :: Parser Token Goal a
goal = Goal <$> position <*> expr <*> localDecls

-- ---------------------------------------------------------------------------
-- Literals, identifiers, and (infix) operators
-- ---------------------------------------------------------------------------

char :: Parser Token Char a
char = cval <$> token CharTok

float :: Parser Token Double a
float = fval <$> token FloatTok

int :: Parser Token Int a
int = fromInteger <$> integer

integer :: Parser Token Integer a
integer = ival <$> token IntTok

string :: Parser Token String a
string = sval <$> token StringTok

tycon :: Parser Token Ident a
tycon = conId

anonOrTyvar :: Parser Token Ident a
anonOrTyvar = anonIdent <|> tyvar

tyvar :: Parser Token Ident a
tyvar = varId

qtycon :: Parser Token QualIdent a
qtycon = qConId

varId :: Parser Token Ident a
varId = ident

funId :: Parser Token Ident a
funId = ident

conId :: Parser Token Ident a
conId = ident

labId :: Parser Token Ident a
labId = renameLabel <$> ident

funSym :: Parser Token Ident a
funSym = sym

conSym :: Parser Token Ident a
conSym = sym

tycls :: Parser Token Ident a
tycls = conId

qtycls :: Parser Token QualIdent a
qtycls = qConId 

modIdent :: Parser Token ModuleIdent a
modIdent = mIdent <?> "module name expected"

var :: Parser Token Ident a
var = varId <|> parens (funSym <?> "operator symbol expected")

fun :: Parser Token Ident a
fun = funId <|> parens (funSym <?> "operator symbol expected")

con :: Parser Token Ident a
con = conId <|> parens (conSym <?> "operator symbol expected")

funop :: Parser Token Ident a
funop = funSym <|> backquotes (funId <?> "operator name expected")

conop :: Parser Token Ident a
conop = conSym <|> backquotes (conId <?> "operator name expected")

qFunId :: Parser Token QualIdent a
qFunId = qIdent

qConId :: Parser Token QualIdent a
qConId = qIdent

qFunSym :: Parser Token QualIdent a
qFunSym = qSym

qConSym :: Parser Token QualIdent a
qConSym = qSym

gConSym :: Parser Token QualIdent a
gConSym = qConSym <|> colon

qfun :: Parser Token QualIdent a
qfun = qFunId <|> parens (qFunSym <?> "operator symbol expected")

qfunop :: Parser Token QualIdent a
qfunop = qFunSym <|> backquotes (qFunId <?> "operator name expected")

gconop :: Parser Token QualIdent a
gconop = gConSym <|> backquotes (qConId <?> "operator name expected")

anonIdent :: Parser Token Ident a
anonIdent = (\ p -> addPositionIdent p anonId) <$> tokenPos Underscore

mIdent :: Parser Token ModuleIdent a
mIdent = mIdent' <$> position <*>
     tokens [Id,QId,Id_as,Id_ccall,Id_forall,Id_hiding,
             Id_interface,Id_interfaceTypeClasses,Id_primitive,
             Id_public, Id_qualified]
  where mIdent' p a = addPositionModuleIdent p $
                      mkMIdent (modulVal a ++ [sval a])

ident :: Parser Token Ident a
ident = (\ pos -> mkIdentPosition pos . sval) <$> position <*>
       tokens [Id,Id_as,Id_ccall,Id_forall,Id_hiding,
               Id_interface,Id_interfaceTypeClasses,Id_primitive,
               Id_public, Id_qualified]

qIdent :: Parser Token QualIdent a
qIdent =  qualify  <$> ident
      <|> mkQIdent' <$> position <*> token QId
  where mkQIdent' p a = qualifyWith (mkMIdent (modulVal a))
                                    (mkIdentPosition p (sval a))

sym :: Parser Token Ident a
sym = (\ pos -> mkIdentPosition pos . sval) <$> position <*>
      tokens [Sym, SymDot, SymMinus, SymMinusDot]

qSym :: Parser Token QualIdent a
qSym = qualify <$> sym <|> mkQIdent' <$> position <*> token QSym
  where mkQIdent' p a = qualifyWith (mkMIdent (modulVal a))
                                    (mkIdentPosition p (sval a))

colon :: Parser Token QualIdent a
colon = (\ p -> qualify $ addPositionIdent p consId) <$> tokenPos Colon

minus :: Parser Token Ident a
minus = (\ p -> addPositionIdent p minusId) <$> tokenPos SymMinus

fminus :: Parser Token Ident a
fminus = (\ p -> addPositionIdent p fminusId) <$> tokenPos SymMinusDot

tupleCommas :: Parser Token QualIdent a
tupleCommas = (\ p -> qualify . addPositionIdent p . tupleId . succ . length)
              <$> position <*> many1 comma

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

layout :: Parser Token a b -> Parser Token a b
layout p =  layoutOff <-*> between leftBraceSemicolon p rightBrace
        <|> layoutOn  <-*> p <*-> (token VRightBrace <|> layoutEnd)

-- ---------------------------------------------------------------------------
-- Bracket combinators
-- ---------------------------------------------------------------------------

braces :: Parser Token a b -> Parser Token a b
braces p = between leftBrace p rightBrace

brackets :: Parser Token a b -> Parser Token a b
brackets p = between leftBracket p rightBracket

parens :: Parser Token a b -> Parser Token a b
parens p = between leftParen p rightParen

backquotes :: Parser Token a b -> Parser Token a b
backquotes p = between backquote p checkBackquote

-- ---------------------------------------------------------------------------
-- Simple token parsers
-- ---------------------------------------------------------------------------

token :: Category -> Parser Token Attributes a
token c = attr <$> symbol (Token c NoAttributes)
  where attr (Token _ a) = a

tokens :: [Category] -> Parser Token Attributes a
tokens = foldr1 (<|>) . map token

tokenPos :: Category -> Parser Token Position a
tokenPos c = position <*-> token c

tokenOps :: [(Category,a)] -> Parser Token a b
tokenOps cs = ops [(Token c NoAttributes, x) | (c, x) <- cs]

comma :: Parser Token Attributes a
comma = token Comma

semicolon :: Parser Token Attributes a
semicolon = token Semicolon <|> token VSemicolon

bar :: Parser Token Attributes a
bar = token Bar

checkBar :: Parser Token Attributes a
checkBar = bar <?> "| expected"

equals :: Parser Token Attributes a
equals = token Equals

checkEquals :: Parser Token Attributes a
checkEquals = equals <?> "= expected"

checkWhere :: Parser Token Attributes a
checkWhere = token KW_where <?> "where expected"

checkSelect :: Parser Token Attributes a
checkSelect = token Select <?> ":-> expected"

checkRightArrow :: Parser Token Attributes a
checkRightArrow  = token RightArrow <?> "-> expected"

checkBind :: Parser Token Attributes a
checkBind = token Bind <?> ":= expected"

backquote :: Parser Token Attributes a
backquote = token Backquote

checkBackquote :: Parser Token Attributes a
checkBackquote = backquote <?> "backquote (`) expected"

leftParen :: Parser Token Attributes a
leftParen = token LeftParen

rightParen :: Parser Token Attributes a
rightParen = token RightParen

leftBracket :: Parser Token Attributes a
leftBracket = token LeftBracket

rightBracket :: Parser Token Attributes a
rightBracket = token RightBracket

leftBrace :: Parser Token Attributes a
leftBrace = token LeftBrace

leftBraceSemicolon :: Parser Token Attributes a
leftBraceSemicolon = token LeftBraceSemicolon

rightBrace :: Parser Token Attributes a
rightBrace = token RightBrace

leftArrow :: Parser Token Attributes a
leftArrow = token LeftArrow

-- ---------------------------------------------------------------------------
-- Ident
-- ---------------------------------------------------------------------------

mkIdentPosition :: Position -> String -> Ident
mkIdentPosition pos = addPositionIdent pos . mkIdent

-- ---------------------------------------------------------------------------
-- Type classes
-- ---------------------------------------------------------------------------

-- |Simple class context, consisting of pairs (class, type variable)
scontext :: Parser Token SContext a
scontext = SContext <$> ((parens (sepBy simpleclass comma)) <|> ((:[]) <$> simpleclass))

emptyscontext :: SContext
emptyscontext = SContext [] 

simpleclass :: Parser Token (QualIdent, Ident) a
simpleclass = (,) <$> qtycls <*> tyvar

-- |Type in instance declaration. Also recognizes special syntax forms
-- for tuples, lists and function types. 
inst :: Parser Token (TypeConstructor, [Ident]) a
inst = 
  -- qualified type constructor or special type constructor 
  -- ( "()", "[]", "->", "(,{,})" ) standing alone
  (\tcon -> (tcon,[])) <$> gtycon 
  -- (<qualified type constructor or special type constructor> {tvars})
  <|?> parens ((,) <$> gtycon <*> many tyvar)
  -- Tuple (tvar1, ..., tvarn)
  <|?> (\vars -> (TupleTC (length vars), vars)) 
         <$> parens ((:) <$> tyvar <*> many1 (comma <-*> tyvar))
  -- List [tvar]
  <|?> (\lvar -> (ListTC, [lvar])) <$> brackets tyvar
  -- Function type (tvar1 -> tvar2)
  <|?> (\(v1, v2) -> (ArrowTC, [v1, v2])) 
         <$> parens ((,) <$> (tyvar <*-> token RightArrow) <*> tyvar) 

-- | any (qualfied) type constructor, and the special type constructors
-- (), [], (->) and (,{,})
gtycon :: Parser Token TypeConstructor a
gtycon = QualTC <$> qtycon 
    <|?> UnitTC <$-> unit
    <|?> ListTC <$-> emptyList
    <|?> ArrowTC <$-> funArrow
    <|?> TupleTC <$> tupleTC

unit :: Parser Token () a
unit = parens (succeed ())

emptyList :: Parser Token () a
emptyList = brackets (succeed ()) 

funArrow :: Parser Token () a
funArrow = () <$-> leftParen <*-> token RightArrow <*-> rightParen

-- | a tuple type constructor, i.e. "(,)", "(,,)", "(,,,)", ...
tupleTC :: Parser Token Int a
tupleTC = ((+1) . length) <$> parens (many1 comma)

-- | a saturated context (not the simple context as above)   
context :: Parser Token Context a
context = (\c -> Context [c]) <$> contextElem 
      <|> Context <$> (parens $ (contextElem `sepBy` comma))  

-- | each saturated context element has the following form:
-- contextElem ::= qtycls tyvar | qtycls (tyvar atype_1 ... atype_n) n >= 1
contextElem :: Parser Token ContextElem a
contextElem = (\tcon (tvar, types) -> ContextElem tcon tvar types) 
  <$> qtycon <*> 
  ((,) <$> tyvar <*> succeed [] <|> parens ((,) <$> tyvar <*> many1 atype))

atype :: Parser Token TypeExpr a
atype = gtycon' <|?> identType <|?> parenType False <|?> listType False


gtycon' :: Parser Token TypeExpr a
gtycon' = {-(flip ConstructorType []) <$> qtycon
     <|?> SpecialConstructorType UnitTC [] <$-> unit
     <|?> -}SpecialConstructorType ListTC [] <$-> emptyList
     <|?> SpecialConstructorType ArrowTC [] <$-> funArrow
     <|?> (flip SpecialConstructorType  []) <$> (TupleTC <$> tupleTC) 



