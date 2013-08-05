{- |
    Module      :  $Header$
    Description :  Interface for reading and manipulating Curry source code
    Copyright   :  (c) 2009        Holger Siegel
                       2011 - 2012 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}
module Curry.Syntax
  ( module Curry.Syntax.Type
  , module Curry.Syntax.Utils
  , L.Token (..), L.Category (..), L.Attributes (..)
  , lexSource
  , parseInterface, parseHeader, parseModule, parseGoal
  , ppModule, ppInterface, ppIDecl
  , showModule
  ) where

import           Curry.Base.Position          (Position)
import           Curry.Base.Message           (MessageM)
import           Curry.Files.Unlit            (unlit)

import qualified Curry.Syntax.Lexer      as L
import qualified Curry.Syntax.Parser     as P
import           Curry.Syntax.Pretty          (ppModule, ppInterface, ppIDecl)
import           Curry.Syntax.ShowModule      (showModule)
import           Curry.Syntax.Type
import           Curry.Syntax.Utils

-- |Return the result of a lexical analysis of the source program @src@.
-- The result is a list of tuples consisting of a 'Position' and a 'Token'.
lexSource :: FilePath -> String -> MessageM [(Position, L.Token)]
lexSource fn src = unlit fn src >>= L.lexSource fn

-- |Parse a Curry 'Interface'
parseInterface :: FilePath -> String -> MessageM [Interface]
parseInterface = P.parseInterface

-- |Parse a Curry 'Module' header
parseHeader :: FilePath -> String -> MessageM Module
parseHeader fn src = unlit fn src >>= P.parseHeader fn

-- |Parse a Curry 'Module'
parseModule :: FilePath -> String -> MessageM Module
parseModule fn src = unlit fn src >>= P.parseSource fn

-- |Parse a 'Goal', i.e. an expression with (optional) local declarations
parseGoal :: String -> MessageM Goal
parseGoal = P.parseGoal
