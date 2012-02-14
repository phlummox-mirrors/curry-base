{- |
    Module      :  $Header$
    Description :  Positions in a source file
    Copyright   :  (c) Wolfgang Lux
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  non-portable (DeriveDataTypeable)

    This module implements a data type for positions in a source file and
    respective functions to operate on them. A source file position consists
    of a filename, a line number, and a column number. A tab stop is assumed
    at every eighth column.

    In addition, the type 'SrcRef' identifies the path to an expression in
    the abstract syntax tree by argument positions, which is used for
    debugging purposes.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Curry.Base.Position
  ( -- * Source code position
    Position (..), tabWidth, first, incr, next, tab, nl, showLine, incPosition

    -- * source reference
  , SrcRef (..), SrcRefOf (..), noRef, incSrcRef
  ) where

import Data.Generics (Data(..), Typeable (..))
import System.FilePath

-- |Source code positions
data Position
  -- |Normal source code position
  = Position
    { file   :: FilePath -- ^ 'FilePath' of the source file
    , line   :: Int      -- ^ line number, beginning at 1
    , column :: Int      -- ^ column number, beginning at 1
    , astRef :: SrcRef   -- ^ reference to the abstract syntax tree
    }
  -- |Position in the abstract syntax tree
  | AST
    { astRef :: SrcRef -- ^ reference to the abstract syntax tree
    }
  -- |no position
  | NoPos
    deriving (Eq, Ord, Data, Typeable)

instance Read Position where
  readsPrec p s =
    [ (Position "" i j noRef, s') | ((i, j), s') <- readsPrec p s ]

instance Show Position where
  showsPrec _ (Position f l c _)
    = (if null f then id else showString (normalise f) . showString ", ")
    . showString "line " . shows l
    . (if c > 0 then showChar '.' . shows c else id)
  showsPrec _ (AST _) = id
  showsPrec _ NoPos   = id

instance SrcRefOf Position where
  srcRefOf NoPos = noRef
  srcRefOf x     = astRef x

-- |Number of spaces for a tabulator
tabWidth :: Int
tabWidth = 8

-- | Absolute first position of a file
first :: FilePath -> Position
first fn = Position fn 1 1 noRef

-- |Increment a position by a number of columns
incr :: Position -> Int -> Position
incr p@Position { column = c } n = p { column = c + n }
incr p _ = p

-- |Next position to the right
next :: Position -> Position
next = flip incr 1

-- |First position after the next tabulator
tab :: Position -> Position
tab p@Position { column = c }
  = p { column = c + tabWidth - (c - 1) `mod` tabWidth }
tab p = p

-- |First position of the next line
nl :: Position -> Position
nl p@Position { line = l } = p { line = l + 1, column = 1 }
nl p = p

-- |Increment the position in the abstract syntax tree
incPosition :: Position -> Int -> Position
incPosition NoPos _ = NoPos
incPosition p     j = p { astRef = incSrcRef (astRef p) j }

-- |Show the line and column of the 'Position'
showLine :: Position -> String
showLine NoPos  = ""
showLine AST {} = ""
showLine Position { line = l, column = c }
  = "(line " ++ show l ++ "." ++ show c ++ ")"

-- ---------------------------------------------------------------------------
-- A source reference is a reference to a position in the abstract syntax tree
-- used for debugging purposes.
-- ---------------------------------------------------------------------------

-- |A pointer to the origin
newtype SrcRef = SrcRef [Int] deriving (Data, Typeable)

-- ---------------------------------------------------------------------------
-- The instances for standard classes or such that SrcRefs are invisible
-- ---------------------------------------------------------------------------

instance Eq SrcRef
  where _ == _ = True

instance Ord SrcRef
  where compare _ _ = EQ

instance Read SrcRef where
  readsPrec _ s = [(noRef, s)]

instance Show SrcRef where
  show _ = ""

-- |Type class for data types containing source code references
class SrcRefOf a where
  -- |Retrieve all 'SrcRef's
  srcRefsOf :: a -> [SrcRef]
  srcRefsOf = (: []) . srcRefOf

  -- |Retrieve the first 'SrcRef'
  srcRefOf :: a -> SrcRef
  srcRefOf = head . srcRefsOf

-- |The empty source code reference
noRef :: SrcRef
noRef = SrcRef []

-- |Increment a source code reference by a given number
incSrcRef :: SrcRef -> Int -> SrcRef
incSrcRef (SrcRef [i]) j = SrcRef [i + j]
incSrcRef is  _ = error $ "Curry.Base.Position.incSrcRef: " ++ show is
