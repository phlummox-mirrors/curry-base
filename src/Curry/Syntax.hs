{- |
    Module      :  $Header$
    Description :  Interface for reading and manipulating Curry source code
    Copyright   :  (c) 2009        Holger Siegel
                       2011 - 2013 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}
module Curry.Syntax
  ( module Curry.Syntax.Type
  , module Curry.Syntax.Utils
  , L.Token (..), L.Category (..), L.Attributes (..)
  , unlit, unlitLexSource, unlitParseHeader, unlitParseModule
  , lexSource, parseInterface, parseHeader, parseModule, parseGoal
  , ppModule, ppInterface, ppIDecl
  , showModule
  ) where

import           Curry.Base.Position          (Position)
import           Curry.Base.Message           (MessageM)
import qualified Curry.Files.Unlit       as U (unlit)

import qualified Curry.Syntax.Lexer      as L
import qualified Curry.Syntax.Parser     as P
import           Curry.Syntax.Pretty          (ppModule, ppInterface, ppIDecl)
import           Curry.Syntax.ShowModule      (showModule)
import           Curry.Syntax.Type
import           Curry.Syntax.Utils

-- |Unliterate a LiterateCurry file, identity on normal Curry file.
unlit :: FilePath -> String -> MessageM String
unlit = U.unlit

-- |Unliterate and return the result of a lexical analysis of the source
-- program @src@.
-- The result is a list of tuples consisting of a 'Position' and a 'Token'.
unlitLexSource :: FilePath -> String -> MessageM [(Position, L.Token)]
unlitLexSource fn src = unlit fn src >>= lexSource fn

-- |Unliterate and parse a Curry 'Module' header
unlitParseHeader :: FilePath -> String -> MessageM Module
unlitParseHeader fn src = unlit fn src >>= parseHeader fn

-- |Unliterate and parse a Curry 'Module'
unlitParseModule :: FilePath -> String -> MessageM Module
unlitParseModule fn src = unlit fn src >>= parseModule fn

-- |Return the result of a lexical analysis of the source program @src@.
-- The result is a list of tuples consisting of a 'Position' and a 'Token'.
lexSource :: FilePath -> String -> MessageM [(Position, L.Token)]
lexSource = L.lexSource

-- |Parse a Curry 'Interface'
parseInterface :: FilePath -> String -> MessageM Interface
parseInterface = P.parseInterface

-- |Parse a Curry 'Module' header
parseHeader :: FilePath -> String -> MessageM Module
parseHeader = P.parseHeader

-- |Parse a Curry 'Module'
parseModule :: FilePath -> String -> MessageM Module
parseModule = P.parseSource

-- |Parse a 'Goal', i.e. an expression with (optional) local declarations
parseGoal :: String -> MessageM Goal
parseGoal = P.parseGoal
