{- |
    Module      :  $Header$
    Description :  A lexer for Curry
    Copyright   :  (c) 1999-2004, Wolfgang Lux
                       Martin Engelke, Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}
module Curry.Syntax.Lexer
  ( -- * Data types for tokens
    Token (..), Category (..), Attributes (..)

    -- * lexing functions
  , lexFile, lexer, fullLexer
  ) where

import Prelude hiding (fail)
import Data.Char
  ( chr, ord, isAlpha, isAlphaNum, isSpace, isUpper, isDigit, isOctDigit
  , isHexDigit)
import Data.List (intercalate)
import qualified Data.Map as Map (Map, union, lookup, fromList)

import Curry.Base.LexComb
import Curry.Base.Position

-- ---------------------------------------------------------------------------
-- Tokens; note that the equality and ordering instances of Token disregard
-- the attributes.
-- TODO@bjp: Think about a Token representation with arguments instead of the
-- category.
-- ---------------------------------------------------------------------------

-- |Data type for curry lexer tokens
data Token = Token Category Attributes

instance Eq Token where
  Token c1 _ == Token c2 _ = c1 == c2

instance Ord Token where
  Token c1 _ `compare` Token c2 _ = c1 `compare` c2

instance Symbol Token where
  isEOF (Token c _) = c == EOF

-- |Category of curry tokens
data Category
  -- literals
  = CharTok
  | IntTok
  | FloatTok
  | IntegerTok
  | StringTok

  -- identifiers
  | Id   -- identifier
  | QId  -- qualified identifier
  | Sym  -- symbol
  | QSym -- qualified symbol

  -- punctuation symbols
  | LeftParen     -- (
  | RightParen    -- )
  | Semicolon     -- ;
  | LeftBrace     -- {
  | RightBrace    -- }
  | LeftBracket   -- [
  | RightBracket  -- ]
  | Comma         -- ,
  | Underscore    -- _
  | Backquote     -- `

  -- layout (inserted by bbr)
  | LeftBraceSemicolon -- {; (turn off layout)
  | VSemicolon         -- virtual ;
  | VRightBrace        -- virtual }

  -- reserved keywords
  | KW_case
--  | KW_class -- not supported yet
  | KW_choice -- deprecated
  | KW_data
--  | KW_deriving -- not supported yet
  | KW_do
  | KW_else
  | KW_eval -- deprecated
  | KW_external
  | KW_free
  | KW_if
  | KW_import
  | KW_in
  | KW_infix
  | KW_infixl
  | KW_infixr
--  | KW_instance -- not supported yet
  | KW_let
  | KW_module
  | KW_newtype
  | KW_of
  | KW_rigid -- deprecated
  | KW_then
  | KW_type
  | KW_where

  -- reserved operators
  | At           -- @
  | Colon        -- :
  | DotDot       -- ..
  | DoubleColon  -- ::
  | Equals       -- =
  | Backslash    -- \
  | Bar          -- |
  | LeftArrow    -- <-
  | RightArrow   -- ->
  | Tilde        -- ~
  | Binds        -- :=
--  | Context      -- => -- not supported yet

  -- special identifiers
  | Id_as
  | Id_ccall
  | Id_forall
  | Id_hiding
  | Id_interface
  | Id_primitive
  | Id_qualified

  -- special operators
  | SymDot      -- .
  | SymMinus    -- -
  | SymMinusDot -- -.

  -- compiler pragma (bjp)
  | Pragma

  -- comments (only for full lexer) inserted by men & bbr
  | LineComment
  | NestedComment

  -- end-of-file token
  | EOF
    deriving (Eq, Ord)

-- There are different kinds of attributes associated with the tokens.
-- Most attributes simply save the string corresponding to the token.
-- However, for qualified identifiers, we also record the list of module
-- qualifiers. The values corresponding to a literal token are properly
-- converted already. To simplify the creation and extraction of
-- attribute values, we make use of records.

-- |Attributes associated to a token
data Attributes
  = NoAttributes
  | CharAttributes    { cval     :: Char    , original :: String}
  | IntAttributes     { ival     :: Int     , original :: String}
  | FloatAttributes   { fval     :: Double  , original :: String}
  | IntegerAttributes { intval   :: Integer , original :: String}
  | StringAttributes  { sval     :: String  , original :: String}
  | IdentAttributes   { modulVal :: [String], sval     :: String}

instance Show Attributes where
  showsPrec _ NoAttributes             = showChar '_'
  showsPrec _ (CharAttributes    cv _) = shows cv
  showsPrec _ (IntAttributes     iv _) = shows iv
  showsPrec _ (FloatAttributes   fv _) = shows fv
  showsPrec _ (IntegerAttributes iv _) = shows iv
  showsPrec _ (StringAttributes  sv _) = shows sv
  showsPrec _ (IdentAttributes  mid i) = showsEscaped
                                       $ intercalate "." $ mid ++ [i]

-- ---------------------------------------------------------------------------
-- The following functions can be used to construct tokens with
-- specific attributes.
-- ---------------------------------------------------------------------------

-- |Construct a simple 'Token' without 'Attributes'
tok :: Category -> Token
tok t = Token t NoAttributes

-- |Construct a 'Token' for identifiers
idTok :: Category -> [String] -> String -> Token
idTok t mIdent ident = Token t
  IdentAttributes { modulVal = mIdent, sval = ident }

-- |Construct a 'Token' for a single 'Char'
charTok :: Char -> String -> Token
charTok c o = Token CharTok CharAttributes { cval = c, original = o }

-- |Construct a 'Token' for an int value
intTok :: Int -> String -> Token
intTok base digits =
  Token IntTok IntAttributes { ival = convertIntegral base digits
                             , original = digits }

-- |Construct a 'Token' for a float value
floatTok :: String -> String -> Int -> String -> Token
floatTok mant frac expo rest =
  Token FloatTok FloatAttributes { fval = convertFloating mant frac expo
                                 , original = mant ++ "." ++ frac ++ rest }

-- |Construct a 'Token' for an integer value
integerTok :: Integer -> String -> Token
integerTok base digts = Token IntegerTok
  IntegerAttributes { intval = convertIntegral base digts, original = digts }

-- |Construct a 'Token' for a string value
stringTok :: String -> String -> Token
stringTok cs s = Token StringTok
  StringAttributes { sval = cs, original = s }

-- |Construct a 'Token' for a line comment
lineCommentTok :: String -> Token
lineCommentTok s = Token LineComment
  StringAttributes { sval = s, original = s }

-- |Construct a 'Token' for a nested comment
nestedCommentTok :: String -> Token
nestedCommentTok s = Token NestedComment
  StringAttributes { sval = s, original = s }

-- ---------------------------------------------------------------------------
-- The 'Show' instance of 'Token' is designed to display all tokens in their
-- source representation.
-- ---------------------------------------------------------------------------

showsEscaped :: String -> ShowS
showsEscaped s = showChar '`' . showString s . showChar '\''

showsIdent :: Attributes -> ShowS
showsIdent a = showString "identifier " . shows a

showsSpecialIdent :: String -> ShowS
showsSpecialIdent s = showString "identifier " . showsEscaped s

showsOperator :: Attributes -> ShowS
showsOperator a = showString "operator " . shows a

showsSpecialOperator :: String -> ShowS
showsSpecialOperator s = showString "operator " . showsEscaped s

instance Show Token where
  showsPrec _ (Token Id         a) = showsIdent a
  showsPrec _ (Token QId        a) = showString "qualified " . showsIdent a
  showsPrec _ (Token Sym        a) = showsOperator a
  showsPrec _ (Token QSym       a) = showString "qualified " . showsOperator a
  showsPrec _ (Token IntTok     a) = showString "integer "   . shows a
  showsPrec _ (Token FloatTok   a) = showString "float "     . shows a
  showsPrec _ (Token CharTok    a) = showString "character " . shows a
  showsPrec _ (Token IntegerTok a) = showString "integer "   . shows a
  showsPrec _ (Token StringTok  a) = showString "string "    . shows a
  showsPrec _ (Token LeftParen          _) = showsEscaped "("
  showsPrec _ (Token RightParen         _) = showsEscaped ")"
  showsPrec _ (Token Semicolon          _) = showsEscaped ";"
  showsPrec _ (Token LeftBrace          _) = showsEscaped "{"
  showsPrec _ (Token RightBrace         _) = showsEscaped "}"
  showsPrec _ (Token LeftBracket        _) = showsEscaped "["
  showsPrec _ (Token RightBracket       _) = showsEscaped "]"
  showsPrec _ (Token Comma              _) = showsEscaped ","
  showsPrec _ (Token Underscore         _) = showsEscaped "_"
  showsPrec _ (Token Backquote          _) = showsEscaped "`"
  showsPrec _ (Token LeftBraceSemicolon _)
    = showsEscaped "{;" . showString " (turn off layout)"
  showsPrec _ (Token VSemicolon         _)
    = showsEscaped ";" . showString " (inserted due to layout)"
  showsPrec _ (Token VRightBrace        _)
    = showsEscaped "}" . showString " (inserted due to layout)"
  showsPrec _ (Token At                 _) = showsEscaped "@"
  showsPrec _ (Token Colon              _) = showsEscaped ":"
  showsPrec _ (Token DotDot             _) = showsEscaped ".."
  showsPrec _ (Token DoubleColon        _) = showsEscaped "::"
  showsPrec _ (Token Equals             _) = showsEscaped "="
  showsPrec _ (Token Backslash          _) = showsEscaped "\\"
  showsPrec _ (Token Bar                _) = showsEscaped "|"
  showsPrec _ (Token LeftArrow          _) = showsEscaped "<-"
  showsPrec _ (Token RightArrow         _) = showsEscaped "->"
  showsPrec _ (Token Tilde              _) = showsEscaped "~"
  showsPrec _ (Token Binds              _) = showsEscaped ":="
  showsPrec _ (Token SymDot             _) = showsSpecialOperator "."
  showsPrec _ (Token SymMinus           _) = showsSpecialOperator "-"
  showsPrec _ (Token SymMinusDot        _) = showsSpecialOperator "-."
  showsPrec _ (Token KW_case            _) = showsEscaped "case"
  showsPrec _ (Token KW_choice          _) = showsEscaped "choice"
  showsPrec _ (Token KW_data            _) = showsEscaped "data"
  showsPrec _ (Token KW_do              _) = showsEscaped "do"
  showsPrec _ (Token KW_else            _) = showsEscaped "else"
  showsPrec _ (Token KW_eval            _) = showsEscaped "eval"
  showsPrec _ (Token KW_external        _) = showsEscaped "external"
  showsPrec _ (Token KW_free            _) = showsEscaped "free"
  showsPrec _ (Token KW_if              _) = showsEscaped "if"
  showsPrec _ (Token KW_import          _) = showsEscaped "import"
  showsPrec _ (Token KW_in              _) = showsEscaped "in"
  showsPrec _ (Token KW_infix           _) = showsEscaped "infix"
  showsPrec _ (Token KW_infixl          _) = showsEscaped "infixl"
  showsPrec _ (Token KW_infixr          _) = showsEscaped "infixr"
  showsPrec _ (Token KW_let             _) = showsEscaped "let"
  showsPrec _ (Token KW_module          _) = showsEscaped "module"
  showsPrec _ (Token KW_newtype         _) = showsEscaped "newtype"
  showsPrec _ (Token KW_of              _) = showsEscaped "of"
  showsPrec _ (Token KW_rigid           _) = showsEscaped "rigid"
  showsPrec _ (Token KW_then            _) = showsEscaped "then"
  showsPrec _ (Token KW_type            _) = showsEscaped "type"
  showsPrec _ (Token KW_where           _) = showsEscaped "where"
  showsPrec _ (Token Id_as              _) = showsSpecialIdent "as"
  showsPrec _ (Token Id_ccall           _) = showsSpecialIdent "ccall"
  showsPrec _ (Token Id_forall          _) = showsSpecialIdent "forall"
  showsPrec _ (Token Id_hiding          _) = showsSpecialIdent "hiding"
  showsPrec _ (Token Id_interface       _) = showsSpecialIdent "interface"
  showsPrec _ (Token Id_primitive       _) = showsSpecialIdent "primitive"
  showsPrec _ (Token Id_qualified       _) = showsSpecialIdent "qualified"
  showsPrec _ (Token Pragma             a) = shows a
  showsPrec _ (Token LineComment        a) = shows a
  showsPrec _ (Token NestedComment      a) = shows a
  showsPrec _ (Token EOF                _) = showString "<end-of-file>"

-- ---------------------------------------------------------------------------
-- Tables for reserved operators and identifiers
-- ---------------------------------------------------------------------------

-- |Map of reserved operators
reservedOps:: Map.Map String Category
reservedOps = Map.fromList
  [ ("@" , At         )
  , (":" , Colon      )
  , ("::", DoubleColon)
  , ("..", DotDot     )
  , ("=" , Equals     )
  , ("\\", Backslash  )
  , ("|" , Bar        )
  , ("<-", LeftArrow  )
  , ("->", RightArrow )
  , ("~" , Tilde      )
  , (":=", Binds      )
  ]

-- |Map of reserved and special operators
reservedSpecialOps :: Map.Map String Category
reservedSpecialOps = Map.union reservedOps $ Map.fromList
  [ ("." , SymDot     )
  , ("-" , SymMinus   )
  , ("-.", SymMinusDot)
  ]

-- |Map of keywords
keywords :: Map.Map String Category
keywords = Map.fromList
  [ ("case"    , KW_case    )
  , ("choice"  , KW_choice  )
  , ("data"    , KW_data    )
  , ("do"      , KW_do      )
  , ("else"    , KW_else    )
  , ("eval"    , KW_eval    )
  , ("external", KW_external)
  , ("free"    , KW_free    )
  , ("if"      , KW_if      )
  , ("import"  , KW_import  )
  , ("in"      , KW_in      )
  , ("infix"   , KW_infix   )
  , ("infixl"  , KW_infixl  )
  , ("infixr"  , KW_infixr  )
  , ("let"     , KW_let     )
  , ("module"  , KW_module  )
  , ("newtype" , KW_newtype )
  , ("of"      , KW_of      )
  , ("rigid"   , KW_rigid   )
  , ("then"    , KW_then    )
  , ("type"    , KW_type    )
  , ("where"   , KW_where   )
  ]

-- |Map of keywords and special identifiers
keywordsSpecialIds :: Map.Map String Category
keywordsSpecialIds = Map.union keywords $ Map.fromList
  [ ("as"       , Id_as       )
  , ("ccall"    , Id_ccall    )
  , ("forall"   , Id_forall   )
  , ("hiding"   , Id_hiding   )
  , ("interface", Id_interface)
  , ("primitive", Id_primitive)
  , ("qualified", Id_qualified)
  ]

-- ---------------------------------------------------------------------------
-- Character classes
-- ---------------------------------------------------------------------------

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c `elem` "'_"

isSymbol :: Char -> Bool
isSymbol c = c `elem` "~!@#$%^&*+-=<>:?./|\\"

-- ---------------------------------------------------------------------------
-- Lexing functions
-- ---------------------------------------------------------------------------

-- |Lex a given file
lexFile :: FilePath -> String -> MsgMonad [(Position, Token)]
lexFile = parse (applyLexer lexer)

-- |CPS-Lexer for Curry
lexer :: Lexer Token a
lexer = skipWhiteSpace True -- skip whitespace and comments

fullLexer :: Lexer Token a
fullLexer = skipWhiteSpace False -- skip whitespace

skipWhiteSpace :: Bool -> Lexer Token a
skipWhiteSpace skipComments suc fail = skip
  where
  skip p   []          bol = suc p (tok EOF) p [] bol
  skip p   ('\t':s)    bol = skip (tab p) s bol
  skip p   ('\n':s)    _   = skip (nl  p) s True
  skip p c@('-':'-':s) bol
    | skipComments         = skip (nl  p) (safeTail (dropWhile (/= '\n') s))
                             True
    | otherwise            = lexLineComment suc fail p c bol
  skip p c@('{':'-':_) bol = lexNestedComment 0 id p commentSuc fail p c bol
    where commentSuc = if skipComments then (\_ _  -> skip) else suc
  skip p (c:s)         bol
    | isSpace c            = skip (next p) s bol
    | bol                  = lexBOL   suc fail p (c:s) bol
    | otherwise            = lexToken suc fail p (c:s) bol

  safeTail []     = []
  safeTail (_:tl) = tl

lexLineComment :: Lexer Token a
lexLineComment suc _ p s = case break (== '\n') s of
  (cmnt, rest) -> suc p (lineCommentTok cmnt) (incr p $ length cmnt) rest

-- n   : nesting depth
-- comm: comment already lexed as functional list
-- p0  : Start position of comment
lexNestedComment :: Int -> (String -> String) -> Position -> Lexer Token a
lexNestedComment n comm p0 suc fail p str = case (n, str) of
  (_,        []) -> fail p0 "Unterminated nested comment at end-of-file" p []
  (1, '-':'}':s) -> suc  p0 (nestedCommentTok (comm "-}")) (incr p 2) s
  (_, '{':'-':s) -> continue (n+1) ("{-" ++) (incr p 2) s
  (_, '-':'}':s) -> continue (n-1) ("-}" ++) (incr p 2) s
  (_, c@'\t' :s) -> continue n     (c:)      (tab    p) s
  (_, c@'\n' :s) -> continue n     (c:)      (nl     p) s
  (_, c      :s) -> continue n     (c:)      (next   p) s
  where continue n' comm' p' s' = lexNestedComment n' (comm . comm')
                                  p0 suc fail p' s'

lexBOL :: Lexer Token a
lexBOL suc fail p s _ []            = lexToken suc fail p s False []
lexBOL suc fail p s _ ctxt@(n:rest)
  | col <  n  = suc p (tok VRightBrace) p s True  rest
  | col == n  = suc p (tok  VSemicolon) p s False ctxt
  | otherwise = lexToken suc fail p s False ctxt
  where col = column p

lexToken :: Lexer Token a
lexToken suc _    p []    = suc p (tok EOF) p []
lexToken suc fail p (c:s)
  | c == '('    = token LeftParen
  | c == ')'    = token RightParen
  | c == ','    = token Comma
  | c == ';'    = token Semicolon
  | c == '['    = token LeftBracket
  | c == ']'    = token RightBracket
  | c == '_'    = token Underscore
  | c == '`'    = token Backquote
  | c == '{'    = lexLeftBrace (suc p) (next p) s
  | c == '}'    = \bol -> token RightBrace bol . drop 1
  | c == '\''   = lexChar   p suc fail (next p) s
  | c == '\"'   = lexString p suc fail (next p) s
  | isAlpha  c  = lexIdent  (suc p) p (c:s)
  | isSymbol c  = lexSymbol (suc p) p (c:s)
  | isDigit  c  = lexNumber (suc p) p (c:s)
  | otherwise   = fail p ("Illegal character " ++ show c) p s
  where token t = suc p (tok t) (next p) s

lexLeftBrace :: (Token -> P a) -> P a
lexLeftBrace cont p (';':s) = cont (tok LeftBraceSemicolon) (next p) s
lexLeftBrace cont p s       = cont (tok LeftBrace         ) p        s

lexIdent :: (Token -> P a) -> P a
lexIdent cont p s =
  maybe (lexOptQual cont (token Id) [ident]) (cont . token)
        (Map.lookup ident keywordsSpecialIds)
        (incr p (length ident)) rest
  where (ident,rest) = span isIdent s
        token t = idTok t [] ident

lexSymbol :: (Token -> P a) -> P a
lexSymbol cont p s =
  cont (idTok (maybe Sym id (Map.lookup sym reservedSpecialOps)) [] sym)
       (incr p (length sym)) rest
  where (sym,rest) = span isSymbol s

-- ---------------------------------------------------------------------------
-- {\em Note:} the function \texttt{lexOptQual} has been extended to provide
-- the qualified use of the Prelude list operators and tuples.
-- ---------------------------------------------------------------------------

lexOptQual :: (Token -> P a) -> Token -> [String] -> P a
lexOptQual cont token mIdent p ('.':c:s)
  | isAlpha  c       = lexQualIdent cont identCont mIdent (next p) (c:s)
  | isSymbol c       = lexQualSymbol cont identCont mIdent (next p) (c:s)
  | c=='(' || c=='[' = lexQualPreludeSymbol cont token identCont mIdent
                       (next p) (c:s)
  where identCont _ _ = cont token p ('.':c:s)
lexOptQual cont token _      p s = cont token p s

lexQualIdent :: (Token -> P a) -> P a -> [String] -> P a
lexQualIdent cont identCont mIdent p s =
  maybe (lexOptQual cont (idTok QId mIdent ident) (mIdent ++ [ident]))
        (const identCont)
        (Map.lookup ident keywords)
        (incr p (length ident)) rest
  where (ident,rest) = span isIdent s

lexQualSymbol :: (Token -> P a) -> P a -> [String] -> P a
lexQualSymbol cont identCont mIdent p s =
  maybe (cont (idTok QSym mIdent sym)) (const identCont)
        (Map.lookup sym reservedOps)
        (incr p (length sym)) rest
  where (sym,rest) = span isSymbol s

lexQualPreludeSymbol :: (Token -> P a) -> Token -> P a -> [String] -> P a
lexQualPreludeSymbol cont _ _ mIdent p ('[':']':rest) =
  cont (idTok QId mIdent "[]") (incr p 2) rest
lexQualPreludeSymbol cont _ _ mIdent p ('(':rest)
  | not (null rest') && head rest' == ')'
  = cont (idTok QId mIdent ('(':tup++")")) (incr p (length tup+2))
         (tail rest')
  where (tup,rest') = span (== ',') rest
lexQualPreludeSymbol cont token _ _ p s =  cont token p s

-- ---------------------------------------------------------------------------
-- {\em Note:} since Curry allows an unlimited range of integer numbers,
-- read numbers must be converted to Haskell type \texttt{Integer}.
-- ---------------------------------------------------------------------------

lexNumber :: (Token -> P a) -> P a
lexNumber cont p ('0':c:s)
  | c `elem` "oO"  = lexOctal       cont nullCont (incr p 2) s
  | c `elem` "xX"  = lexHexadecimal cont nullCont (incr p 2) s
  where nullCont _ _ = cont (intTok 10 "0") (next p) (c:s)
lexNumber cont p s = lexOptFraction cont (integerTok 10 digits) digits
                     (incr p $ length digits) rest
  where (digits, rest) = span isDigit s

lexOctal :: (Token -> P a) -> P a -> P a
lexOctal cont nullCont p s
  | null digits = nullCont undefined undefined
  | otherwise   = cont (integerTok 8 digits) (incr p $ length digits) rest
  where (digits, rest) = span isOctDigit s

lexHexadecimal :: (Token -> P a) -> P a -> P a
lexHexadecimal cont nullCont p s
  | null digits = nullCont undefined undefined
  | otherwise   = cont (integerTok 16 digits) (incr p $ length digits) rest
  where (digits, rest) = span isHexDigit s

lexOptFraction :: (Token -> P a) -> Token -> String -> P a
lexOptFraction cont _ mant p ('.':c:s)
  | isDigit c = lexOptExponent cont (floatTok mant frac 0 "") mant frac
                               (incr p (length frac+1)) rest
  where (frac,rest) = span isDigit (c:s)
lexOptFraction cont token mant p (c:s)
  | c `elem` "eE" = lexSignedExponent cont intCont mant "" [c] (next p) s
  where intCont _ _ = cont token p (c:s)
lexOptFraction cont token _ p s = cont token p s

lexOptExponent :: (Token -> P a) -> Token -> String -> String -> P a
lexOptExponent cont token mant frac p (c:s)
  | c `elem` "eE" = lexSignedExponent cont floatCont mant frac [c] (next p) s
  where floatCont _ _ = cont token p (c:s)
lexOptExponent cont token _    _    p s = cont token p s

lexSignedExponent :: (Token -> P a) -> P a -> String -> String -> String
                  -> P a
lexSignedExponent cont _         mant frac e p ('+':c:s)
  | isDigit c = lexExponent cont mant frac (e++"+") id (next p) (c:s)
lexSignedExponent cont _         mant frac e p ('-':c:s)
  | isDigit c = lexExponent cont mant frac (e++"-") negate (next p) (c:s)
lexSignedExponent cont _         mant frac e p (c:s)
  | isDigit c = lexExponent cont mant frac e id p (c:s)
lexSignedExponent _    floatCont _     _   _ p s = floatCont p s

lexExponent :: (Token -> P a) -> String -> String -> String -> (Int -> Int)
            -> P a
lexExponent cont mant frac e expSign p s =
  cont (floatTok mant frac expo (e ++ digits)) (incr p (length digits)) rest
  where (digits, rest) = span isDigit s
        expo = expSign (convertIntegral 10 digits)

lexChar :: Position -> Lexer Token a
lexChar p0 _       fail p []    = fail p0 "Illegal character constant" p []
lexChar p0 success fail p (c:s)
  | c == '\\' = lexEscape p (\d o -> lexCharEnd d o p0 success fail)
                          fail (next p) s
  | c == '\n' = fail p0 "Illegal character constant" p (c:s)
  | c == '\t' = lexCharEnd c "\t" p0 success fail (tab  p) s
  | otherwise = lexCharEnd c [c]  p0 success fail (next p) s

lexCharEnd :: Char -> String -> Position -> Lexer Token a
lexCharEnd c o p0 suc _    p ('\'':s) = suc p0 (charTok c o) (next p) s
lexCharEnd _ _ p0 _   fail p s        =
  fail p0 "Improperly terminated character constant" p s

lexString :: Position -> Lexer Token a
lexString p0 success fail = lexStringRest p0 success fail "" id

lexStringRest :: Position -> SuccessP Token a -> FailP a -> String
              -> (String -> String) -> P a
lexStringRest p0 _       fail _  _  p [] =
  fail p0 "Improperly terminated string constant" p []
lexStringRest p0 success fail s0 so p (c:s)
  | c == '\\' =
      lexStringEscape p (lexStringRest p0 success fail) fail s0 so (next p) s
  | c == '\"' = success p0 (stringTok (reverse s0) (so "")) (next p) s
  | c == '\n' = fail p0 "Improperly terminated string constant" p []
  | c == '\t' = lexStringRest p0 success fail (c:s0) (so . (c:)) (tab p) s
  | otherwise = lexStringRest p0 success fail (c:s0) (so . (c:)) (next p) s

lexStringEscape ::  Position -> (String -> (String -> String) -> P a)
                -> FailP a -> String -> (String -> String) -> P a
lexStringEscape p0 _       fail _  _  p [] = lexEscape p0 undefined fail p []
lexStringEscape p0 success fail s0 so p (c:s)
  | c == '&' = success s0 (so . ("\\&"++)) (next p) s
  | isSpace c = lexStringGap (success s0) fail so p (c:s)
  | otherwise = lexEscape p0 (\ c' s' -> success (c':s0) (so . (s'++)))
                          fail p (c:s)

lexStringGap :: ((String -> String) -> P a) -> FailP a -> (String -> String)
             -> P a
lexStringGap _       fail _  p [] = fail p "End of file in string gap" p []
lexStringGap success fail so p (c:s)
  | c == '\\' = success (so . (c:)) (next p) s
  | c == '\t' = lexStringGap success fail (so . (c:)) (tab p) s
  | c == '\n' = lexStringGap success fail (so . (c:)) (nl p) s
  | isSpace c = lexStringGap success fail (so . (c:)) (next p) s
  | otherwise = fail p ("Illegal character in string gap " ++ show c) p s

lexEscape :: Position -> (Char -> String -> P a) -> FailP a -> P a
lexEscape _  suc _    p ('a' :s) = suc '\a' "\\a"  (next p) s
lexEscape _  suc _    p ('b' :s) = suc '\b' "\\b"  (next p) s
lexEscape _  suc _    p ('f' :s) = suc '\f' "\\f"  (next p) s
lexEscape _  suc _    p ('n' :s) = suc '\n' "\\n"  (next p) s
lexEscape _  suc _    p ('r' :s) = suc '\r' "\\r"  (next p) s
lexEscape _  suc _    p ('t' :s) = suc '\t' "\\t"  (next p) s
lexEscape _  suc _    p ('v' :s) = suc '\v' "\\v"  (next p) s
lexEscape _  suc _    p ('\\':s) = suc '\\' "\\\\" (next p) s
lexEscape _  suc _    p ('"' :s) = suc '\"' "\\\"" (next p) s
lexEscape _  suc _    p ('\'':s) = suc '\'' "\\\'" (next p) s
lexEscape _  suc _    p ('^':c:s)
  | isUpper c || c `elem` "@[\\]^_"
  = suc (chr (ord c `mod` 32)) ("\\^"++[c]) (incr p 2) s
lexEscape p0 suc fail p ('o':c:s)
  | isOctDigit c
  = numEscape p0 suc fail 8 isOctDigit ("\\o"++) (next p) (c:s)
lexEscape p0 suc fail p ('x':c:s)
  | isHexDigit c
  = numEscape p0 suc fail 16 isHexDigit ("\\x"++) (next p) (c:s)
lexEscape p0 suc fail p (c:s)
  | isDigit    c
  = numEscape p0 suc fail 10 isDigit ("\\"++) p (c:s)
lexEscape p0 suc fail p s = asciiEscape p0 suc fail p s

asciiEscape :: Position -> (Char -> String -> P a) -> FailP a -> P a
asciiEscape p0 suc fail p str = case str of
  ('N':'U':'L':s) -> suc '\NUL' "\\NUL" (incr p 3) s
  ('S':'O':'H':s) -> suc '\SOH' "\\SOH" (incr p 3) s
  ('S':'T':'X':s) -> suc '\STX' "\\STX" (incr p 3) s
  ('E':'T':'X':s) -> suc '\ETX' "\\ETX" (incr p 3) s
  ('E':'O':'T':s) -> suc '\EOT' "\\EOT" (incr p 3) s
  ('E':'N':'Q':s) -> suc '\ENQ' "\\ENQ" (incr p 3) s
  ('A':'C':'K':s) -> suc '\ACK' "\\ACK" (incr p 3) s
  ('B':'E':'L':s) -> suc '\BEL' "\\BEL" (incr p 3) s
  ('B':'S'    :s) -> suc '\BS'  "\\BS"  (incr p 2) s
  ('H':'T'    :s) -> suc '\HT'  "\\HT"  (incr p 2) s
  ('L':'F'    :s) -> suc '\LF'  "\\LF"  (incr p 2) s
  ('V':'T'    :s) -> suc '\VT'  "\\VT"  (incr p 2) s
  ('F':'F'    :s) -> suc '\FF'  "\\FF"  (incr p 2) s
  ('C':'R'    :s) -> suc '\CR'  "\\CR"  (incr p 2) s
  ('S':'O'    :s) -> suc '\SO'  "\\SO"  (incr p 2) s
  ('S':'I'    :s) -> suc '\SI'  "\\SI"  (incr p 2) s
  ('D':'L':'E':s) -> suc '\DLE' "\\DLE" (incr p 3) s
  ('D':'C':'1':s) -> suc '\DC1' "\\DC1" (incr p 3) s
  ('D':'C':'2':s) -> suc '\DC2' "\\DC2" (incr p 3) s
  ('D':'C':'3':s) -> suc '\DC3' "\\DC3" (incr p 3) s
  ('D':'C':'4':s) -> suc '\DC4' "\\DC4" (incr p 3) s
  ('N':'A':'K':s) -> suc '\NAK' "\\NAK" (incr p 3) s
  ('S':'Y':'N':s) -> suc '\SYN' "\\SYN" (incr p 3) s
  ('E':'T':'B':s) -> suc '\ETB' "\\ETB" (incr p 3) s
  ('C':'A':'N':s) -> suc '\CAN' "\\CAN" (incr p 3) s
  ('E':'M'    :s) -> suc '\EM'  "\\EM"  (incr p 2) s
  ('S':'U':'B':s) -> suc '\SUB' "\\SUB" (incr p 3) s
  ('E':'S':'C':s) -> suc '\ESC' "\\ESC" (incr p 3) s
  ('F':'S'    :s) -> suc '\FS'  "\\FS"  (incr p 2) s
  ('G':'S'    :s) -> suc '\GS'  "\\GS"  (incr p 2) s
  ('R':'S'    :s) -> suc '\RS'  "\\RS"  (incr p 2) s
  ('U':'S'    :s) -> suc '\US'  "\\US"  (incr p 2) s
  ('S':'P'    :s) -> suc '\SP'  "\\SP"  (incr p 2) s
  ('D':'E':'L':s) -> suc '\DEL' "\\DEL" (incr p 3) s
  s               -> fail p0 "Illegal escape sequence" p s

numEscape :: Position -> (Char -> String -> P a) -> FailP a -> Int
          -> (Char -> Bool) -> (String -> String) -> P a
numEscape p0 suc fail b isDigit' so p s
  | n >= minB && n <= maxB = suc (chr n) (so digits)
                                     (incr p (length digits)) rest
  | otherwise              = fail p0 "Numeric escape out-of-range" p s
  where (digits,rest) = span isDigit' s
        n = convertIntegral b digits
        minB = ord minBound
        maxB = ord maxBound

-- OLD STUFF

-- skipNestedComment :: Position -> P a -> FailP a -> P a
-- skipNestedComment p0 _         fail p [] =
--   fail p0 "Unterminated nested comment at end-of-file" p []
-- skipNestedComment _  success _    p ('-':'}':s) = success (incr p 2) s
-- skipNestedComment p0 success fail p ('{':'-':s) =
--   skipNestedComment p (skipNestedComment p0 success fail) fail (incr p 2) s
-- skipNestedComment p0 success fail p ('\t':s) =
--   skipNestedComment p0 success fail (tab p) s
-- skipNestedComment p0 success fail p ('\n':s) =
--   skipNestedComment p0 success fail (nl p) s
-- skipNestedComment p0 success fail p (_:s) =
--   skipNestedComment p0 success fail (next p) s