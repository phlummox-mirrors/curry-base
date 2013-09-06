{- |
    Module      :  $Header$
    Description :  Curry language extensions
    Copyright   :  (c) 2013 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  non-portable (DeriveDataTypeable)

    This module provides the data structures for Curry language extensions.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Curry.Syntax.Extension
  ( -- * Extensions
    Extension (..), KnownExtension (..), classifyExtension, kielExtensions
    -- * Tools
  , Tool (..), classifyTool
  ) where

import Data.Char     (toUpper)
import Data.Generics (Data (..), Typeable (..))

import Curry.Base.Ident    (Ident (..))
import Curry.Base.Position

-- |Data type representing Curry language extensions.
data Extension
  = KnownExtension   Position KnownExtension
  | UnknownExtension Position String
    deriving (Eq, Read, Show, Data, Typeable)

instance HasPosition Extension where
  getPosition (KnownExtension   p _) = p
  getPosition (UnknownExtension p _) = p

  setPosition p (KnownExtension   _ e) = KnownExtension   p e
  setPosition p (UnknownExtension _ e) = UnknownExtension p e

data KnownExtension
  = AnonFreeVars
  | FunctionalPatterns
  | NoImplicitPrelude
  | Records
    deriving (Eq, Read, Show, Enum, Bounded, Data, Typeable)

-- |'Extension's available by Kiel's Curry compilers.
kielExtensions :: [KnownExtension]
kielExtensions = [AnonFreeVars, FunctionalPatterns, Records]

-- |Classifies a 'String' as an 'Extension'
classifyExtension :: Ident -> Extension
classifyExtension i = case reads extName of
  [(e, "")] -> KnownExtension   (idPosition i) e
  _         -> UnknownExtension (idPosition i) extName
  where extName = idName i

-- |Different Curry tools which may accept compiler options.
data Tool = KICS2 | PAKCS | UnknownTool String
    deriving (Eq, Read, Show, Data, Typeable)

-- |Classifies a 'String' as a 'Tool'
classifyTool :: String -> Tool
classifyTool str = case reads (map toUpper str) of
  [(t, "")] -> t
  _         -> UnknownTool str
