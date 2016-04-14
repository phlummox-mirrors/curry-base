{- |
    Module      :  $Header$
    Description :  Curry language extensions
    Copyright   :  (c) 2013 - 2014 Björn Peemöller
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

import Data.Char           (toUpper)
import Data.Generics       (Data, Typeable)

import Curry.Base.Ident    (Ident (..))
import Curry.Base.Position

-- |Specified language extensions, either known or unknown.
data Extension
  = KnownExtension   Position KnownExtension -- ^ a known extension
  | UnknownExtension Position String         -- ^ an unknown extension
    deriving (Eq, Read, Show, Data, Typeable)

instance HasPosition Extension where
  getPosition (KnownExtension   p _) = p
  getPosition (UnknownExtension p _) = p

  setPosition p (KnownExtension   _ e) = KnownExtension   p e
  setPosition p (UnknownExtension _ e) = UnknownExtension p e

-- |Known language extensions of Curry.
data KnownExtension
  = AnonFreeVars              -- ^ anonymous free variables
  | FunctionalPatterns        -- ^ functional patterns
  | NegativeLiterals          -- ^ negative literals
  | NoImplicitPrelude         -- ^ no implicit import of the prelude
  | ExistentialQuantification -- ^ existential quantification 
    deriving (Eq, Read, Show, Enum, Bounded, Data, Typeable)

-- |Classifies a 'String' as an 'Extension'
classifyExtension :: Ident -> Extension
classifyExtension i = case reads extName of
  [(e, "")] -> KnownExtension   (idPosition i) e
  _         -> UnknownExtension (idPosition i) extName
  where extName = idName i

-- |'Extension's available by Kiel's Curry compilers.
kielExtensions :: [KnownExtension]
kielExtensions = [AnonFreeVars, FunctionalPatterns]

-- |Different Curry tools which may accept compiler options.
data Tool = KICS2 | PAKCS | CYMAKE | UnknownTool String
    deriving (Eq, Read, Show, Data, Typeable)

-- |Classifies a 'String' as a 'Tool'
classifyTool :: String -> Tool
classifyTool str = case reads (map toUpper str) of
  [(t, "")] -> t
  _         -> UnknownTool str
