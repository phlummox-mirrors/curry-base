{- |
    Module      :  $Header$
    Description :  A Parser for Curry
    Copyright   :  (c) 1999 - 2004 Wolfgang Lux
                       2005        Martin Engelke
                       2011 - 2015 Björn Peemöller
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
import Curry.Base.Monad       (CYM)
import Curry.Base.Position    (Position, mk, mk')
import Curry.Base.LLParseComb

import Curry.Syntax.Extension
import Curry.Syntax.Lexer (Token (..), Category (..), Attributes (..), lexer)
import Curry.Syntax.Type
import Curry.Syntax.Utils (mkInt, addSrcRefs)

-- |Parse a 'Module'
parseSource :: FilePath -> String -> CYM Module
parseSource fn
  = fmap addSrcRefs
  . fullParser (uncurry <$> moduleHeader <*> layout moduleDecls) lexer fn

-- |Parse a 'Module' header
parseHeader :: FilePath -> String -> CYM Module
parseHeader
  = prefixParser (moduleHeader <*> startLayout importDecls <*> succeed []) lexer
  where importDecls = many (importDecl <*-> many semicolon)

-- |Parse an 'Interface'
parseInterface :: FilePath -> String -> CYM Interface
parseInterface = fullParser interface lexer

-- |Parse a 'Goal'
parseGoal :: String -> CYM Goal
parseGoal = fullParser goal lexer ""

-- ---------------------------------------------------------------------------
-- Module header
-- ---------------------------------------------------------------------------

-- |Parser for a module header
moduleHeader :: Parser a Token ([ImportDecl] -> [Decl] -> Module)
moduleHeader = (\ps (m, es) -> Module ps m es)
           <$> modulePragmas
           <*> header
  where header = (,) <$-> token KW_module <*> modIdent
                     <*>  option exportSpec
                     <*-> expectWhere
                `opt` (mainMIdent, Nothing)

modulePragmas :: Parser a Token [ModulePragma]
modulePragmas = many (languagePragma <|> optionsPragma)

languagePragma :: Parser a Token ModulePragma
languagePragma =   LanguagePragma
              <$>  tokenPos PragmaLanguage
              <*>  (languageExtension `sepBy1` comma)
              <*-> token PragmaEnd
  where languageExtension = classifyExtension <$> ident

optionsPragma :: Parser a Token ModulePragma
optionsPragma = (\pos a -> OptionsPragma pos (fmap classifyTool $ toolVal a)
                                             (toolArgs a))
           <$>  position
           <*>  token PragmaOptions
           <*-> token PragmaEnd

-- |Parser for an export specification
exportSpec :: Parser a Token ExportSpec
exportSpec = Exporting <$> position <*> parens (export `sepBy` comma)

-- |Parser for an export item
export :: Parser a Token Export
export =  qtycon <**> (parens spec `opt` Export)         -- type constructor
      <|> Export <$> qfun <\> qtycon                     -- fun
      <|> ExportModule <$-> token KW_module <*> modIdent -- module
  where spec =       ExportTypeAll  <$-> token DotDot
            <|> flip ExportTypeWith <$>  con `sepBy` comma

moduleDecls :: Parser a Token ([ImportDecl], [Decl])
moduleDecls = impDecl <$> importDecl
                      <*> (semicolon <-*> moduleDecls `opt` ([], []))
          <|> (,) []  <$> topDecls
  where impDecl i (is, ds) = (i:is ,ds)

-- |Parser for a single import declaration
importDecl :: Parser a Token ImportDecl
importDecl =  flip . ImportDecl
          <$> tokenPos KW_import
          <*> flag (token Id_qualified)
          <*> modIdent
          <*> option (token Id_as <-*> modIdent)
          <*> option importSpec

-- |Parser for an import specification
importSpec :: Parser a Token ImportSpec
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

-- |Parser for an interface
interface :: Parser a Token Interface
interface = uncurry <$> intfHeader <*> braces intfDecls

intfHeader :: Parser a Token ([IImportDecl] -> [IDecl] -> Interface)
intfHeader = Interface <$-> token Id_interface <*> modIdent <*-> expectWhere

intfDecls :: Parser a Token ([IImportDecl], [IDecl])
intfDecls = impDecl <$> iImportDecl
                    <*> (semicolon <-*> intfDecls `opt` ([], []))
        <|> (,) [] <$> intfDecl `sepBy` semicolon
  where impDecl i (is, ds) = (i:is, ds)

-- |Parser for a single interface import declaration
iImportDecl :: Parser a Token IImportDecl
iImportDecl = IImportDecl <$> tokenPos KW_import <*> modIdent

-- |Parser for a single interface declaration
intfDecl :: Parser a Token IDecl
intfDecl = choice [ iInfixDecl, iHidingDecl, iDataDecl, iNewtypeDecl
                  , iTypeDecl , iFunctionDecl <\> token Id_hiding ]

-- |Parser for an interface infix declaration
iInfixDecl :: Parser a Token IDecl
iInfixDecl = infixDeclLhs IInfixDecl <*> integer <*> qfunop

-- |Parser for an interface hiding declaration
iHidingDecl :: Parser a Token IDecl
iHidingDecl = tokenPos Id_hiding <**> (hDataDecl <|> hFuncDecl)
  where
  hDataDecl = hiddenData <$-> token KW_data <*> qtycon <*> many tyvar
  hFuncDecl = hidingFunc <$> arity <*-> token DoubleColon <*> type0
  hiddenData tc tvs p = HidingDataDecl p tc tvs
  -- TODO: 0 was inserted to type check, but what is the meaning of this field?
  hidingFunc a ty p = IFunctionDecl p (qualify (mkIdent "hiding")) a ty

-- |Parser for an interface data declaration
iDataDecl :: Parser a Token IDecl
iDataDecl = iTypeDeclLhs IDataDecl KW_data <*> constrs <*> iHidden
  where constrs = equals <-*> constrDecl `sepBy1` bar `opt` []

-- |Parser for an interface newtype declaration
iNewtypeDecl :: Parser a Token IDecl
iNewtypeDecl = iTypeDeclLhs INewtypeDecl KW_newtype
               <*-> equals <*> newConstrDecl <*> iHidden

-- |Parser for an interface type synonym declaration
iTypeDecl :: Parser a Token IDecl
iTypeDecl = iTypeDeclLhs ITypeDecl KW_type
            <*-> equals <*> type0

-- |Parser for an interface hiding pragma
iHidden :: Parser a Token [Ident]
iHidden = token PragmaHiding
          <-*> (con `sepBy` comma)
          <*-> token PragmaEnd
          `opt` []


-- |Parser for an interface function declaration
iFunctionDecl :: Parser a Token IDecl
iFunctionDecl =  IFunctionDecl <$> position <*> qfun <*> arity
            <*-> token DoubleColon <*> type0

-- |Parser for function's arity
arity :: Parser a Token Int
arity = int `opt` 0

iTypeDeclLhs :: (Position -> QualIdent -> [Ident] -> a) -> Category
             -> Parser b Token a
iTypeDeclLhs f kw = f <$> tokenPos kw <*> qtycon <*> many tyvar

-- ---------------------------------------------------------------------------
-- Top-Level Declarations
-- ---------------------------------------------------------------------------

topDecls :: Parser a Token [Decl]
topDecls = topDecl `sepBy` semicolon

topDecl :: Parser a Token Decl
topDecl = choice [ dataDecl, newtypeDecl, typeDecl
                 , foreignDecl, infixDecl, functionDecl ]

dataDecl :: Parser a Token Decl
dataDecl = typeDeclLhs DataDecl KW_data <*> constrs
  where constrs = equals <-*> constrDecl `sepBy1` bar `opt` []

newtypeDecl :: Parser a Token Decl
newtypeDecl = typeDeclLhs NewtypeDecl KW_newtype <*-> equals <*> newConstrDecl

typeDecl :: Parser a Token Decl
typeDecl = typeDeclLhs TypeDecl KW_type <*-> equals <*> type0

typeDeclLhs :: (Position -> Ident -> [Ident] -> a) -> Category
            -> Parser b Token a
typeDeclLhs f kw = f <$> tokenPos kw <*> tycon <*> many anonOrTyvar

constrDecl :: Parser a Token ConstrDecl
constrDecl = position <**> (existVars <**> constr)
  where
  constr =  conId     <**> identDecl
        <|> leftParen <-*> parenDecl
        <|> type1 <\> conId <\> leftParen <**> opDecl
  identDecl =  many type2 <**> (conType <$> opDecl `opt` conDecl)
           <|> recDecl <$> recFields
  parenDecl =  conOpDeclPrefix
           <$> conSym    <*-> rightParen <*> type2 <*> type2
           <|> tupleType <*-> rightParen <**> opDecl
  opDecl = conOpDecl <$> conop <*> type1
  recFields                        = layoutOff <-*> braces
                                       (fieldDecl `sepBy` comma)
  conType f tys c                  = f $ ConstructorType (qualify c) tys
  conDecl tys c tvs p              = ConstrDecl p tvs c tys
  conOpDecl op ty2 ty1 tvs p       = ConOpDecl p tvs ty1 op ty2
  conOpDeclPrefix op ty1 ty2 tvs p = ConOpDecl p tvs ty1 op ty2
  recDecl fs c tvs p               = RecordDecl p tvs c fs

fieldDecl :: Parser a Token FieldDecl
fieldDecl = FieldDecl <$> position <*> labels <*-> token DoubleColon <*> type0
  where labels = fun `sepBy1` comma

newConstrDecl :: Parser a Token NewConstrDecl
newConstrDecl = position <**> (existVars <**> (con <**> newConstr))
  where newConstr =  newConDecl <$> type2
                 <|> newRecDecl <$> newFieldDecl
        newConDecl ty  c vs p = NewConstrDecl p vs c ty
        newRecDecl fld c vs p = NewRecordDecl p vs c fld

newFieldDecl :: Parser a Token (Ident, TypeExpr)
newFieldDecl = layoutOff <-*> braces labelDecl
  where labelDecl = (,) <$> fun <*-> token DoubleColon <*> type0

-- Parsing of existential variables (currently disabled)
existVars :: Parser a Token [Ident]
{-
existVars flat
  | flat = succeed []
  | otherwise = token Id_forall <-*> many1 tyvar <*-> dot `opt` []
-}
existVars = succeed []

functionDecl :: Parser a Token Decl
functionDecl = position <**> decl
  where
  decl = fun `sepBy1` comma <**> funListDecl
    <|?> mkFunDecl <$> lhs <*> declRhs
  lhs = (\f -> (f, FunLhs f [])) <$> fun <|?> funLhs

funListDecl :: Parser a Token ([Ident] -> Position -> Decl)
funListDecl =  typeSig           <$-> token DoubleColon <*> type0
           <|> flip ExternalDecl <$-> token KW_external
  where typeSig ty vs p = TypeSig p vs ty

mkFunDecl :: (Ident, Lhs) -> Rhs -> Position -> Decl
mkFunDecl (f, lhs) rhs' p = FunctionDecl p f [Equation p lhs rhs']

funLhs :: Parser a Token (Ident, Lhs)
funLhs = mkFunLhs    <$> fun      <*> many1 pattern2
    <|?> flip ($ id) <$> pattern1 <*> opLhs
    <|?> curriedLhs
  where
  opLhs  =                opLHS funSym (gConSym <\> funSym)
       <|> backquote <-*> opLHS (funId            <*-> expectBackquote)
                                (qConId <\> funId <*-> expectBackquote)
  opLHS funP consP = mkOpLhs    <$> funP  <*> pattern0
                 <|> mkInfixPat <$> consP <*> pattern1 <*> opLhs
  mkFunLhs f ts           = (f , FunLhs f ts)
  mkOpLhs op t2 f t1      = (op, OpLhs (f t1) op t2)
  mkInfixPat op t2 f g t1 = f (g . InfixPattern t1 op) t2

curriedLhs :: Parser a Token (Ident,Lhs)
curriedLhs = apLhs <$> parens funLhs <*> many1 pattern2
  where apLhs (f, lhs) ts = (f, ApLhs lhs ts)

declRhs :: Parser a Token Rhs
declRhs = rhs equals

rhs :: Parser a Token b -> Parser a Token Rhs
rhs eq = rhsExpr <*> localDecls
  where rhsExpr =  SimpleRhs  <$-> eq <*> position <*> expr
               <|> GuardedRhs <$>  many1 (condExpr eq)

localDecls :: Parser a Token [Decl]
localDecls = token KW_where <-*> layout valueDecls `opt` []

valueDecls :: Parser a Token [Decl]
valueDecls  = choice [infixDecl, valueDecl, foreignDecl] `sepBy` semicolon

infixDecl :: Parser a Token Decl
infixDecl = infixDeclLhs InfixDecl <*> option integer <*> funop `sepBy1` comma

infixDeclLhs :: (Position -> Infix -> a) -> Parser b Token a
infixDeclLhs f = f <$> position <*> tokenOps infixKW
  where infixKW = [(KW_infix, Infix), (KW_infixl, InfixL), (KW_infixr, InfixR)]

valueDecl :: Parser a Token Decl
valueDecl = position <**> decl
  where
  decl =   var `sepBy1` comma         <**> valListDecl
      <|?> patOrFunDecl <$> pattern0   <*> declRhs
      <|?> mkFunDecl    <$> curriedLhs <*> declRhs

  valListDecl = funListDecl <|> flip FreeDecl <$-> token KW_free

  patOrFunDecl (ConstructorPattern c ts)
    | not (isConstrId c) = mkFunDecl (f, FunLhs f ts)
    where f = unqualify c
  patOrFunDecl t = patOrOpDecl id t

  patOrOpDecl f (InfixPattern t1 op t2)
    | isConstrId op = patOrOpDecl (f . InfixPattern t1 op) t2
    | otherwise     = mkFunDecl (op', OpLhs (f t1) op' t2)
    where op' = unqualify op
  patOrOpDecl f t = mkPatDecl (f t)

  mkPatDecl t rhs' p = PatternDecl p t rhs'

  isConstrId c = c == qConsId || isQualified c || isQTupleId c

foreignDecl :: Parser a Token Decl
foreignDecl = ForeignDecl
          <$> tokenPos KW_foreign
          <*> callConv <*> (option string)
          <*> fun <*-> token DoubleColon <*> type0
  where callConv =  CallConvPrimitive <$-> token Id_primitive
                <|> CallConvCCall     <$-> token Id_ccall
                <?> "Unsupported calling convention"

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- type0 ::= type1 ['->' type0]
type0 :: Parser a Token TypeExpr
type0 = type1 `chainr1` (ArrowType <$-> token RightArrow)

-- type1 ::= QTyCon { type2 } | type2
type1 :: Parser a Token TypeExpr
type1 = ConstructorType <$> qtycon <*> many type2
     <|> type2 <\> qtycon

-- type2 ::= anonType | identType | parenType | listType
type2 :: Parser a Token TypeExpr
type2 = anonType <|> identType <|> parenType <|> listType

-- anonType ::= '_'
anonType :: Parser a Token TypeExpr
anonType = VariableType <$> anonIdent

-- identType ::= <identifier>
identType :: Parser a Token TypeExpr
identType = VariableType <$> tyvar
        <|> flip ConstructorType [] <$> qtycon <\> tyvar

-- parenType ::= '(' tupleType ')'
parenType :: Parser a Token TypeExpr
parenType = parens tupleType

-- tupleType ::= type0                         (parenthesized type)
--            |  type0 ',' type0 { ',' type0 } (tuple type)
--            |                                (unit type)
tupleType :: Parser a Token TypeExpr
tupleType = type0 <**> (tuple <$> many1 (comma <-*> type0) `opt` ParenType)
            `opt` TupleType []
  where tuple tys ty = TupleType (ty : tys)

-- listType ::= '[' type0 ']'
listType :: Parser a Token TypeExpr
listType = ListType <$> brackets type0

-- ---------------------------------------------------------------------------
-- Literals
-- ---------------------------------------------------------------------------

-- literal ::= '\'' <escaped character> '\''
--          |  <integer>
--          |  <float>
--          |  '"' <escaped string> '"'
literal :: Parser a Token Literal
literal = mk Char   <$> char
      <|> mkInt     <$> integer
      <|> mk Float  <$> float
      <|> mk String <$> string

-- ---------------------------------------------------------------------------
-- Patterns
-- ---------------------------------------------------------------------------

-- pattern0 ::= pattern1 [ gconop pattern0 ]
pattern0 :: Parser a Token Pattern
pattern0 = pattern1 `chainr1` (flip InfixPattern <$> gconop)

-- pattern1 ::= varId
--           |  QConId { pattern2 }
--           |  '-'  Integer
--           |  '-.' Float
--           |  '(' parenPattern'
--           | pattern2
pattern1 :: Parser a Token Pattern
pattern1 =  varId <**> identPattern'            -- unqualified
        <|> qConId <\> varId <**> constrPattern -- qualified
        <|> minus     <**> negNum
        <|> fminus    <**> negFloat
        <|> leftParen <-*> parenPattern'
        <|> pattern2  <\> qConId <\> leftParen
  where
  identPattern' =  optAsRecPattern
               <|> mkConsPattern qualify <$> many1 pattern2

  constrPattern =  mkConsPattern id <$> many1 pattern2
               <|> optRecPattern

  mkConsPattern f ts c = ConstructorPattern (f c) ts

  parenPattern' =  minus  <**> minusPattern negNum
               <|> fminus <**> minusPattern negFloat
               <|> gconPattern
               <|> funSym <\> minus <\> fminus <*-> rightParen
                                               <**> identPattern'
               <|> parenTuplePattern <\> minus <\> fminus <*-> rightParen
  minusPattern p = rightParen           <-*> identPattern' -- (-) and (-.) as variables
                <|> parenMinusPattern p <*-> rightParen
  gconPattern = ConstructorPattern <$> gconId <*-> rightParen
                                   <*> many pattern2

pattern2 :: Parser a Token Pattern
pattern2 =  literalPattern <|> anonPattern <|> identPattern
        <|> parenPattern   <|> listPattern <|> lazyPattern

-- literalPattern ::= <integer> | <char> | <float> | <string>
literalPattern :: Parser a Token Pattern
literalPattern = LiteralPattern <$> literal

-- anonPattern ::= '_'
anonPattern :: Parser a Token Pattern
anonPattern = VariablePattern <$> anonIdent

-- identPattern ::= Variable [ '@' pattern2 | '{' fields '}'
--               |  qConId   [ '{' fields '}' ]
identPattern :: Parser a Token Pattern
identPattern =  varId <**> optAsRecPattern -- unqualified
            <|> qConId <\> varId <**> optRecPattern               -- qualified

-- TODO: document me!
parenPattern :: Parser a Token Pattern
parenPattern = leftParen <-*> parenPattern'
  where
  parenPattern' = minus  <**> minusPattern negNum
              <|> fminus <**> minusPattern negFloat
              <|> flip ConstructorPattern [] <$> gconId <*-> rightParen
              <|> funSym <\> minus <\> fminus <*-> rightParen
                                              <**> optAsRecPattern
              <|> parenTuplePattern <\> minus <\> fminus <*-> rightParen
  minusPattern p = rightParen <-*> optAsRecPattern
                <|> parenMinusPattern p <*-> rightParen

-- listPattern ::= '[' pattern0s ']'
-- pattern0s   ::= {- empty -}
--              |  pattern0 ',' pattern0s
listPattern :: Parser a Token Pattern
listPattern = mk' ListPattern <$> brackets (pattern0 `sepBy` comma)

-- lazyPattern ::= '~' pattern2
lazyPattern :: Parser a Token Pattern
lazyPattern = mk LazyPattern <$-> token Tilde <*> pattern2

-- optRecPattern ::= [ '{' fields '}' ]
optRecPattern :: Parser a Token (QualIdent -> Pattern)
optRecPattern = mkRecPattern <$> fields pattern0 `opt` mkConPattern
  where
  mkRecPattern fs c = RecordPattern c fs
  mkConPattern c    = ConstructorPattern c []

-- ---------------------------------------------------------------------------
-- Partial patterns used in the combinators above, but also for parsing
-- the left-hand side of a declaration.
-- ---------------------------------------------------------------------------

gconId :: Parser a Token QualIdent
gconId = colon <|> tupleCommas

negNum :: Parser a Token (Ident -> Pattern)
negNum = flip NegativePattern <$> (mkInt <$> integer <|> mk Float <$> float)

negFloat :: Parser a Token (Ident -> Pattern)
negFloat = flip NegativePattern . mk Float
           <$> (fromIntegral <$> integer <|> float)

optAsRecPattern :: Parser a Token (Ident -> Pattern)
optAsRecPattern =  flip AsPattern <$-> token At <*> pattern2
               <|> recPattern     <$>  fields pattern0
               `opt` VariablePattern
  where recPattern fs v = RecordPattern (qualify v) fs

optInfixPattern :: Parser a Token (Pattern -> Pattern)
optInfixPattern = mkInfixPat <$> gconop <*> pattern0
            `opt` id
  where mkInfixPat op t2 t1 = InfixPattern t1 op t2

optTuplePattern :: Parser a Token (Pattern -> Pattern)
optTuplePattern = tuple <$> many1 (comma <-*> pattern0)
            `opt` ParenPattern
  where tuple ts t = mk TuplePattern (t:ts)

parenMinusPattern :: Parser a Token (Ident -> Pattern)
                  -> Parser a Token (Ident -> Pattern)
parenMinusPattern p = p <.> optInfixPattern <.> optTuplePattern

parenTuplePattern :: Parser a Token Pattern
parenTuplePattern = pattern0 <**> optTuplePattern
              `opt` mk TuplePattern []

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- condExpr ::= '|' expr0 eq expr
--
-- Note: The guard is an `expr0` instead of `expr` since conditional expressions
-- may also occur in case expressions, and an expression like
-- @
-- case a of { _ -> True :: Bool -> a }
-- @
-- can not be parsed with a limited parser lookahead.
condExpr :: Parser a Token b -> Parser a Token CondExpr
condExpr eq = CondExpr <$> position <*-> bar <*> expr0 <*-> eq <*> expr

-- expr ::= expr0 [ '::' type0 ]
expr :: Parser a Token Expression
expr = expr0 <??> (flip Typed <$-> token DoubleColon <*> type0)

-- expr0 ::= expr1 { infixOp expr1 }
expr0 :: Parser a Token Expression
expr0 = expr1 `chainr1` (flip InfixApply <$> infixOp)

-- expr1 ::= - expr2 | -. expr2 | expr2
expr1 :: Parser a Token Expression
expr1 =  UnaryMinus <$> (minus <|> fminus) <*> expr2
     <|> expr2

-- expr2 ::= lambdaExpr | letExpr | doExpr | ifExpr | caseExpr | expr3
expr2 :: Parser a Token Expression
expr2 = choice [ lambdaExpr, letExpr, doExpr, ifExpr, caseExpr
               , foldl1 Apply <$> many1 expr3
               ]

expr3 :: Parser a Token Expression
expr3 = foldl RecordUpdate <$> expr4 <*> many recUpdate
  where recUpdate = layoutOff <-*> braces (field expr0 `sepBy1` comma)

expr4 :: Parser a Token Expression
expr4 = choice
  [constant, anonFreeVariable, variable, parenExpr, listExpr]

constant :: Parser a Token Expression
constant = Literal <$> literal

anonFreeVariable :: Parser a Token Expression
anonFreeVariable =  (\ p v -> Variable $ qualify $ addPositionIdent p v)
                <$> position <*> anonIdent

variable :: Parser a Token Expression
variable = qFunId <**> optRecord
  where optRecord = flip Record <$> fields expr0 `opt` Variable

parenExpr :: Parser a Token Expression
parenExpr = parens pExpr
  where
  pExpr = (minus <|> fminus) <**> minusOrTuple
      <|> Constructor <$> tupleCommas
      <|> leftSectionOrTuple <\> minus <\> fminus
      <|> opOrRightSection <\> minus <\> fminus
      `opt` mk Tuple []
  minusOrTuple = flip UnaryMinus <$> expr1 <.> infixOrTuple
            `opt` Variable . qualify
  leftSectionOrTuple = expr1 <**> infixOrTuple
  infixOrTuple = ($ id) <$> infixOrTuple'
  infixOrTuple' = infixOp <**> leftSectionOrExp
              <|> (.) <$> (optType <.> tupleExpr)
  leftSectionOrExp = expr1 <**> (infixApp <$> infixOrTuple')
                `opt` leftSection
  optType   = flip Typed <$-> token DoubleColon <*> type0 `opt` id
  tupleExpr = tuple <$> many1 (comma <-*> expr) `opt` Paren
  opOrRightSection =  qFunSym <**> optRightSection
                  <|> colon   <**> optCRightSection
                  <|> infixOp <\> colon <\> qFunSym <**> rightSection
  optRightSection  = (. InfixOp    ) <$> rightSection `opt` Variable
  optCRightSection = (. InfixConstr) <$> rightSection `opt` Constructor
  rightSection     = flip RightSection <$> expr0
  infixApp f e2 op g e1 = f (g . InfixApply e1 op) e2
  leftSection op f e = LeftSection (f e) op
  tuple es e = mk Tuple (e:es)

infixOp :: Parser a Token InfixOp
infixOp = InfixOp <$> qfunop <|> InfixConstr <$> colon

listExpr :: Parser a Token Expression
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

lambdaExpr :: Parser a Token Expression
lambdaExpr = mk Lambda <$-> token Backslash <*> many1 pattern2
                       <*-> expectRightArrow <*> expr

letExpr :: Parser a Token Expression
letExpr = Let <$-> token KW_let <*> layout valueDecls
              <*-> (token KW_in <?> "in expected") <*> expr

doExpr :: Parser a Token Expression
doExpr = uncurry Do <$-> token KW_do <*> layout stmts

ifExpr :: Parser a Token Expression
ifExpr = mk IfThenElse
    <$-> token KW_if                         <*> expr
    <*-> (token KW_then <?> "then expected") <*> expr
    <*-> (token KW_else <?> "else expected") <*> expr

caseExpr :: Parser a Token Expression
caseExpr = keyword <*> expr
      <*-> (token KW_of <?> "of expected") <*> layout (alt `sepBy1` semicolon)
  where keyword =  mk Case Flex  <$-> token KW_fcase
               <|> mk Case Rigid <$-> token KW_case

alt :: Parser a Token Alt
alt = Alt <$> position <*> pattern0 <*> rhs expectRightArrow

fields :: Parser a Token b -> Parser a Token [Field b]
fields p = layoutOff <-*> braces (field p `sepBy` comma)

field :: Parser a Token b -> Parser a Token (Field b)
field p = Field <$> position <*> qfun <*-> expectEquals <*> p

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

stmts :: Parser a Token ([Statement], Expression)
stmts = stmt reqStmts optStmts

reqStmts :: Parser a Token (Statement -> ([Statement], Expression))
reqStmts = (\ (sts, e) st -> (st : sts, e)) <$-> semicolon <*> stmts

optStmts :: Parser a Token (Expression -> ([Statement],Expression))
optStmts = succeed (mk StmtExpr) <.> reqStmts `opt` (,) []

quals :: Parser a Token [Statement]
quals = stmt (succeed id) (succeed $ mk StmtExpr) `sepBy1` comma

stmt :: Parser a Token (Statement -> b)
     -> Parser a Token (Expression -> b) -> Parser a Token b
stmt stmtCont exprCont =  letStmt stmtCont exprCont
                      <|> exprOrBindStmt stmtCont exprCont

letStmt :: Parser a Token (Statement -> b)
        -> Parser a Token (Expression -> b) -> Parser a Token b
letStmt stmtCont exprCont = token KW_let <-*> layout valueDecls <**> optExpr
  where optExpr =  flip Let <$-> token KW_in <*> expr <.> exprCont
               <|> succeed StmtDecl <.> stmtCont

exprOrBindStmt :: Parser a Token (Statement -> b)
               -> Parser a Token (Expression -> b)
               -> Parser a Token b
exprOrBindStmt stmtCont exprCont =
       mk StmtBind <$> pattern0 <*-> leftArrow <*> expr <**> stmtCont
  <|?> expr <\> token KW_let <**> exprCont

-- ---------------------------------------------------------------------------
-- Goals
-- ---------------------------------------------------------------------------

goal :: Parser a Token Goal
goal = Goal <$> position <*> expr <*> localDecls

-- ---------------------------------------------------------------------------
-- Literals, identifiers, and (infix) operators
-- ---------------------------------------------------------------------------

char :: Parser a Token Char
char = cval <$> token CharTok

float :: Parser a Token Double
float = fval <$> token FloatTok

int :: Parser a Token Int
int = fromInteger <$> integer

integer :: Parser a Token Integer
integer = ival <$> token IntTok

string :: Parser a Token String
string = sval <$> token StringTok

tycon :: Parser a Token Ident
tycon = conId

anonOrTyvar :: Parser a Token Ident
anonOrTyvar = anonIdent <|> tyvar

tyvar :: Parser a Token Ident
tyvar = varId

qtycon :: Parser a Token QualIdent
qtycon = qConId

varId :: Parser a Token Ident
varId = ident

funId :: Parser a Token Ident
funId = ident

conId :: Parser a Token Ident
conId = ident

funSym :: Parser a Token Ident
funSym = sym

conSym :: Parser a Token Ident
conSym = sym

modIdent :: Parser a Token ModuleIdent
modIdent = mIdent <?> "module name expected"

var :: Parser a Token Ident
var = varId <|> parens (funSym <?> "operator symbol expected")

fun :: Parser a Token Ident
fun = funId <|> parens (funSym <?> "operator symbol expected")

con :: Parser a Token Ident
con = conId <|> parens (conSym <?> "operator symbol expected")

funop :: Parser a Token Ident
funop = funSym <|> backquotes (funId <?> "operator name expected")

conop :: Parser a Token Ident
conop = conSym <|> backquotes (conId <?> "operator name expected")

qFunId :: Parser a Token QualIdent
qFunId = qIdent

qConId :: Parser a Token QualIdent
qConId = qIdent

qFunSym :: Parser a Token QualIdent
qFunSym = qSym

qConSym :: Parser a Token QualIdent
qConSym = qSym

gConSym :: Parser a Token QualIdent
gConSym = qConSym <|> colon

qfun :: Parser a Token QualIdent
qfun = qFunId <|> parens (qFunSym <?> "operator symbol expected")

qfunop :: Parser a Token QualIdent
qfunop = qFunSym <|> backquotes (qFunId <?> "operator name expected")

gconop :: Parser a Token QualIdent
gconop = gConSym <|> backquotes (qConId <?> "operator name expected")

anonIdent :: Parser a Token Ident
anonIdent = (\ p -> addPositionIdent p anonId) <$> tokenPos Underscore

mIdent :: Parser a Token ModuleIdent
mIdent = mIdent' <$> position <*>
     tokens [Id,QId,Id_as,Id_ccall,Id_forall,Id_hiding,
             Id_interface,Id_primitive,Id_qualified]
  where mIdent' p a = addPositionModuleIdent p $
                      mkMIdent (modulVal a ++ [sval a])

ident :: Parser a Token Ident
ident = (\ pos -> mkIdentPosition pos . sval) <$> position <*>
       tokens [Id,Id_as,Id_ccall,Id_forall,Id_hiding,
               Id_interface,Id_primitive,Id_qualified]

qIdent :: Parser a Token QualIdent
qIdent =  qualify  <$> ident
      <|> mkQIdent <$> position <*> token QId
  where mkQIdent p a = qualifyWith (mkMIdent (modulVal a))
                                   (mkIdentPosition p (sval a))

sym :: Parser a Token Ident
sym = (\ pos -> mkIdentPosition pos . sval) <$> position <*>
      tokens [Sym, SymDot, SymMinus, SymMinusDot]

qSym :: Parser a Token QualIdent
qSym = qualify <$> sym <|> mkQIdent <$> position <*> token QSym
  where mkQIdent p a = qualifyWith (mkMIdent (modulVal a))
                                   (mkIdentPosition p (sval a))

colon :: Parser a Token QualIdent
colon = (\ p -> qualify $ addPositionIdent p consId) <$> tokenPos Colon

minus :: Parser a Token Ident
minus = (\ p -> addPositionIdent p minusId) <$> tokenPos SymMinus

fminus :: Parser a Token Ident
fminus = (\ p -> addPositionIdent p fminusId) <$> tokenPos SymMinusDot

tupleCommas :: Parser a Token QualIdent
tupleCommas = (\ p -> qualify . addPositionIdent p . tupleId . succ . length)
              <$> position <*> many1 comma

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- |This function starts a new layout block but does not wait for its end.
-- This is only used for parsing the module header.
startLayout :: Parser a Token b -> Parser a Token b
startLayout p = layoutOff <-*> leftBrace <-*> p
             <|> layoutOn <-*> p

layout :: Parser a Token b -> Parser a Token b
layout p =  layoutOff <-*> braces p
        <|> layoutOn  <-*> p <*-> (token VRightBrace <|> layoutEnd)

-- ---------------------------------------------------------------------------
-- Bracket combinators
-- ---------------------------------------------------------------------------

braces :: Parser a Token b -> Parser a Token b
braces p = between leftBrace p rightBrace

brackets :: Parser a Token b -> Parser a Token b
brackets p = between leftBracket p rightBracket

parens :: Parser a Token b -> Parser a Token b
parens p = between leftParen p rightParen

backquotes :: Parser a Token b -> Parser a Token b
backquotes p = between backquote p expectBackquote

-- ---------------------------------------------------------------------------
-- Simple token parsers
-- ---------------------------------------------------------------------------

token :: Category -> Parser a Token Attributes
token c = attr <$> symbol (Token c NoAttributes)
  where attr (Token _ a) = a

tokens :: [Category] -> Parser a Token Attributes
tokens = foldr1 (<|>) . map token

tokenPos :: Category -> Parser a Token Position
tokenPos c = position <*-> token c

tokenOps :: [(Category, b)] -> Parser a Token b
tokenOps cs = ops [(Token c NoAttributes, x) | (c, x) <- cs]

comma :: Parser a Token Attributes
comma = token Comma

semicolon :: Parser a Token Attributes
semicolon = token Semicolon <|> token VSemicolon

bar :: Parser a Token Attributes
bar = token Bar

equals :: Parser a Token Attributes
equals = token Equals

expectEquals :: Parser a Token Attributes
expectEquals = equals <?> "= expected"

expectWhere :: Parser a Token Attributes
expectWhere = token KW_where <?> "where expected"

expectRightArrow :: Parser a Token Attributes
expectRightArrow  = token RightArrow <?> "-> expected"

backquote :: Parser a Token Attributes
backquote = token Backquote

expectBackquote :: Parser a Token Attributes
expectBackquote = backquote <?> "backquote (`) expected"

leftParen :: Parser a Token Attributes
leftParen = token LeftParen

rightParen :: Parser a Token Attributes
rightParen = token RightParen

leftBracket :: Parser a Token Attributes
leftBracket = token LeftBracket

rightBracket :: Parser a Token Attributes
rightBracket = token RightBracket

leftBrace :: Parser a Token Attributes
leftBrace = token LeftBrace

rightBrace :: Parser a Token Attributes
rightBrace = token RightBrace

leftArrow :: Parser a Token Attributes
leftArrow = token LeftArrow

-- ---------------------------------------------------------------------------
-- Ident
-- ---------------------------------------------------------------------------

mkIdentPosition :: Position -> String -> Ident
mkIdentPosition pos = addPositionIdent pos . mkIdent
