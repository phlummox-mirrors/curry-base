{- |
    Module      :  $Header$
    Description :  Lexer combinators
    Copyright   :  (c) 1999-2004, Wolfgang Lux
                       2012     , Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module provides the basic types and combinators to implement the
    lexers. The combinators use continuation passing code in a monadic style.

    The first argument of the continuation function is the current position,
    and the second is the string to be parsed. The third argument is a flag
    which signals the lexer that it is lexing the beginning of a line and
    therefore has to check for layout tokens. The fourth argument is a stack
    of indentations that is used to handle nested layout groups.
-}

module Curry.Base.LexComb
  ( -- * Types
    Symbol (..), Indent, Context, P, MessageM, SuccessP, FailP, Lexer

    -- * Monadic functions
  , parse, applyLexer, returnP, thenP, thenP_, failP, liftP, closeP0, closeP1

    -- * Combinators for layout handling
  , pushContext, popContext

    -- * Conversion of numbers
  , convertSignedIntegral, convertSignedFloating
  , convertIntegral, convertFloating
  ) where

import Data.Char           (isDigit, isUpper, ord)

import Curry.Base.Message  (MessageM, failWithAt)
import Curry.Base.Position (Position, first)


infixl 1 `thenP`, `thenP_`

-- |Type class for symbols
class (Ord s, Show s) => Symbol s where
  -- |Does the 'Symbol' represent the end of the input?
  isEOF :: s -> Bool

-- |Type for indentations, necessary for the layout rule
type Indent = Int

-- |Type of context for representing layout grouping
type Context = [Indent]

-- |Basic lexer function
type P a = Position   -- ^ Current source code position
        -> String     -- ^ 'String' to be parsed
        -> Bool       -- ^ Flag whether the beginning of a line should be
                      --   parsed, which requires layout checking
        -> Context    -- ^ context as a stack of 'Indent's
        -> MessageM a

-- |Apply a lexer on a 'String' to lex the content. The second parameter
-- requires a 'FilePath' to use in the 'Position'
parse :: P a -> FilePath -> String -> MessageM a
parse p fn s = p (first fn) s False []

-- ---------------------------------------------------------------------------
-- CPS lexer
-- ---------------------------------------------------------------------------

-- |success continuation
type SuccessP s a = Position -> s -> P a

-- |failure continuation
type FailP a      = Position -> String -> P a

-- |A CPS lexer
type Lexer s a    = SuccessP s a -> FailP a -> P a

-- |Apply a lexer
applyLexer :: Symbol s => Lexer s [(Position, s)] -> P [(Position, s)]
applyLexer lexer = lexer successP failP
  where successP p t | isEOF t   = returnP [(p, t)]
                     | otherwise = ((p, t) :) `liftP` lexer successP failP

-- ---------------------------------------------------------------------------
-- Monadic functions for the lexer.
-- ---------------------------------------------------------------------------

-- |Lift a value into the lexer type
returnP :: a -> P a
returnP x _ _ _ _ = return x

-- |Apply the first lexer and then apply the second one, based on the result
-- of the first lexer.
thenP :: P a -> (a -> P b) -> P b
thenP lexer k pos s bol ctxt
  = lexer pos s bol ctxt >>= \x -> k x pos s bol ctxt

-- |Apply the first lexer and then apply the second one, ignoring the first
-- result.
thenP_ :: P a -> P b -> P b
p1 `thenP_` p2 = p1 `thenP` \_ -> p2

-- |Fail to lex on a 'Position', given an error message
failP :: Position -> String -> P a
failP pos msg _ _ _ _ = failWithAt pos msg

-- |Apply a pure function to the lexers result
liftP :: (a -> b) -> P a -> P b
liftP f p = p `thenP` returnP . f

-- |Lift a lexer into the 'P' monad, returning the lexer when evaluated.
closeP0 :: P a -> P (P a)
closeP0 lexer pos s bol ctxt = return (\_ _ _ _ -> lexer pos s bol ctxt)

-- |Lift a lexer-generating function into the 'P' monad, returning the
--  function when evaluated.
closeP1 :: (a -> P b) -> P (a -> P b)
closeP1 f pos s bol ctxt = return (\x _ _ _ _ -> f x pos s bol ctxt)

-- ---------------------------------------------------------------------------
-- Combinators for handling layout.
-- ---------------------------------------------------------------------------

-- |Push an 'Indent' to the context, increasing the levels of indentation
pushContext :: Indent -> P a -> P a
pushContext col cont pos s bol ctxt = cont pos s bol (col : ctxt)

-- |Pop an 'Indent' from the context, decreasing the levels of indentation
popContext :: P a -> P a
popContext cont pos s bol (_ : ctxt) = cont pos s bol ctxt
popContext _    pos _ _   []         = failWithAt pos $
  "Parse error: popping layout from empty context stack. " ++
  "Perhaps you have inserted too many '}'?"

-- ---------------------------------------------------------------------------
-- Conversions from 'String's into numbers.
-- ---------------------------------------------------------------------------

-- |Convert a String into a signed intergral using a given base
convertSignedIntegral :: Num a => a -> String -> a
convertSignedIntegral b ('+':s) = convertIntegral b s
convertSignedIntegral b ('-':s) = - convertIntegral b s
convertSignedIntegral b s       = convertIntegral b s

-- |Convert a String into an unsigned intergral using a given base
convertIntegral :: Num a => a -> String -> a
convertIntegral b = foldl op 0
  where m `op` n | isDigit n = b * m + fromIntegral (ord n - ord0)
                 | isUpper n = b * m + fromIntegral (ord n - ordA)
                 | otherwise = b * m + fromIntegral (ord n - orda)
        ord0 = ord '0'
        ordA = ord 'A' - 10
        orda = ord 'a' - 10

-- |Convert a mantissa, a fraction part and an exponent into a signed
-- floating value
convertSignedFloating :: Fractional a => String -> String -> Int -> a
convertSignedFloating ('+':m) f e =   convertFloating m f e
convertSignedFloating ('-':m) f e = - convertFloating m f e
convertSignedFloating m       f e =   convertFloating m f e

-- |Convert a mantissa, a fraction part and an exponent into an unsigned
-- floating value
convertFloating :: Fractional a => String -> String -> Int -> a
convertFloating m f e
  | e' == 0   = m'
  | e' >  0   = m' * 10 ^ e'
  | otherwise = m' / 10 ^ (- e')
  where m' = convertIntegral 10 (m ++ f)
        e' = e - length f
