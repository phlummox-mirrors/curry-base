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
    Extension (..), classifyExtension
    -- * Extension groups
  , kielExtensions, knownExtensions
    -- * Tools
  , Tool (..), classifyTool
  ) where

import Data.Char     (toUpper)
import Data.Generics (Data (..), Typeable (..))

-- |Data type representing Curry language extensions.
data Extension
  = KnownExtension KnownExtension
  | UnknownExtension String
    deriving (Eq, Read, Show, Data, Typeable)

data KnownExtension
  = FunctionalPatterns
  | NoImplicitPrelude
  | Records
    deriving (Eq, Read, Show, Enum, Bounded, Data, Typeable)

-- |List of all known 'Extension's.
knownExtensions :: [KnownExtension]
knownExtensions = [ minBound .. maxBound ]

-- |'Extension's available by Kiel's Curry compilers.
kielExtensions :: [KnownExtension]
kielExtensions = [FunctionalPatterns, Records]

-- |Classifies a 'String' as an 'Extension'
classifyExtension :: String -> Extension
classifyExtension str = case reads str of
  [(e, "")] -> KnownExtension e
  _         -> UnknownExtension str

-- |Different Curry tools which may accept compiler options.
data Tool = KICS | KICS2 | MCC | PAKCS | UnknownTool String
    deriving (Eq, Read, Show, Data, Typeable)

-- |Classifies a 'String' as a 'Tool'
classifyTool :: String -> Tool
classifyTool str = case reads (map toUpper str) of
  [(t, "")] -> t
  _         -> UnknownTool str
