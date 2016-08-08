{- |
    Module      :  $Header$
    Description :  Spans in a source file
    Copyright   :  (c) 2016 Jan Tikovsky
    License     :  OtherLicense

    Maintainer  :  jrt@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  non-portable (DeriveDataTypeable)

    This module implements a data type for span information in a source file and
    respective functions to operate on them. A source file span consists
    of a filename, a start position and an end position.

    In addition, the type 'SrcRef' identifies the path to an expression in
    the abstract syntax tree by argument positions, which is used for
    debugging purposes.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Curry.Base.Span where

import Data.Generics     (Data, Typeable)
import System.FilePath

import Curry.Base.Position hiding (astRef)
import Curry.Base.Pretty

data Span
  -- |Normal source code span
  = Span
    { file     :: FilePath -- ^ 'FilePath' of the source file
    , start    :: Position -- ^ start position
    , end      :: Position -- ^ end position
    , astRef   :: SrcRef   -- ^ reference to the abstract syntax tree
    }
  -- |Span in the abstract syntax tree
  | ASTSpan
    { astRef :: SrcRef -- ^ reference to the abstract syntax tree
    }
  -- |no span
  | NoSpan
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance SrcRefOf Span where
  srcRefOf NoSpan = noRef
  srcRefOf x      = astRef x

instance Pretty Span where
  pPrint = ppSpan

-- |Show a 'Span' as a 'String'
showSpan :: Span -> String
showSpan = show . ppSpan

-- |Pretty print a 'Span'
ppSpan :: Span -> Doc
ppSpan s@(Span f _ _ _)
  | null f    = startEnd
  | otherwise = text (normalise f) <> comma <+> startEnd
  where startEnd = ppPositions s
ppSpan _ = empty

-- |Pretty print the start and end position of a 'Span'
ppPositions :: Span -> Doc
ppPositions (Span _ s e _) =  text "startPos:" <+> ppLine s <> comma
                          <+> text "endPos:"   <+> ppLine e
ppPositions _              = empty

fstSpan :: FilePath -> Span
fstSpan fn = Span fn (first fn) (first fn) noRef

-- |Compute the column of the start position of a 'Span'
startCol :: Span -> Int
startCol (Span _ p _ _) = column p
startCol _              = 0

nextSpan :: Span -> Span
nextSpan sp = incrSpan sp 1

incrSpan :: Span -> Int -> Span
incrSpan (Span fn s e ref) n = Span fn (incr s n) (incr e n) ref
incrSpan sp                _ = sp

-- TODO: Rename to tab and nl as soon as positions are completely replaced by spans

-- |Convert a span to a (start) position
-- TODO: This function should be removed as soon as positions are completely replaced by spans
-- in the frontend
span2Pos :: Span -> Position
span2Pos (Span _ p _ _) = p
span2Pos (ASTSpan  ref) = AST ref
span2Pos NoSpan         = NoPos

-- |First position after the next tabulator
tabSpan :: Span -> Span
tabSpan (Span fn s e ref) = Span fn (tab s) (tab e) ref
tabSpan sp                = sp

-- |First position of the next line
nlSpan :: Span -> Span
nlSpan (Span fn s e ref) = Span fn (nl s) (nl e) ref
nlSpan sp                = sp

-- |Distance of a span, i.e. the line and column distance between start
-- and end position
type Distance = (Int, Int)

-- |Set the distance of a span, i.e. update its end position
setDistance :: Span -> Distance -> Span
setDistance (Span fn p _ ref) d = Span fn p (p `moveBy` d) ref
setDistance s                 _ = s

-- |Move position by given distance
moveBy :: Position -> Distance -> Position
moveBy (Position fn l c ref) (ld, cd) = Position fn (l + ld) (c + cd) ref
moveBy p                     _        = p
