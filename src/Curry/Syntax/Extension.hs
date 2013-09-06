{- |
    Module      :  $Header$
    Description :  Curry language extensions
    Copyright   :  (c) 2012 Björn Peemöller
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

-- |Data type representing Curry language extensions
data Extension
  = BangPatterns
  | FunctionalPatterns
  | Newtypes
  | NoImplicitPrelude
  | Records
  | UnknownExtension String
    deriving (Eq, Read, Show, Data, Typeable)

knownExtensions :: [Extension]
knownExtensions =
  [ BangPatterns
  , FunctionalPatterns
  , Newtypes
  , NoImplicitPrelude
  , Records
  ]

-- |'Extension's available by Kiel's Curry compilers
kielExtensions :: [Extension]
kielExtensions = [FunctionalPatterns, Records]

-- |Classifies a 'String' as an 'Extension'
classifyExtension :: String -> Extension
classifyExtension str = case reads str of
  [(e, "")] -> e
  _         -> UnknownExtension str

data Tool = KICS | KICS2 | MCC | PAKCS | UnknownTool String
    deriving (Eq, Read, Show, Data, Typeable)

-- |Classifies a 'String' as an 'Extension'
classifyTool :: String -> Tool
classifyTool str = case reads (map toUpper str) of
  [(t, "")] -> t
  _         -> UnknownTool str
