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
import           Curry.Base.Message           (Message, runMsg)
import qualified Curry.Files.Unlit       as U (unlit)

import qualified Curry.Syntax.Lexer      as L
import qualified Curry.Syntax.Parser     as P
import           Curry.Syntax.Pretty          (ppModule, ppInterface, ppIDecl)
import           Curry.Syntax.ShowModule      (showModule)
import           Curry.Syntax.Type
import           Curry.Syntax.Utils

-- |Unliterate a LiterateCurry file, identity on normal Curry file.
unlit :: FilePath -> String -> Either Message String
unlit fn src = runMsg (U.unlit fn src)

-- |Unliterate and return the result of a lexical analysis of the source
-- program @src@.
-- The result is a list of tuples consisting of a 'Position' and a 'Token'.
unlitLexSource :: FilePath -> String -> Either Message [(Position, L.Token)]
unlitLexSource fn src = runMsg $ U.unlit fn src >>= L.lexSource fn

-- |Unliterate and parse a Curry 'Module' header
unlitParseHeader :: FilePath -> String -> Either Message Module
unlitParseHeader fn src = runMsg $ U.unlit fn src >>= P.parseHeader fn

-- |Unliterate and parse a Curry 'Module'
unlitParseModule :: FilePath -> String -> Either Message Module
unlitParseModule fn src = runMsg $ U.unlit fn src >>= P.parseSource fn

-- |Return the result of a lexical analysis of the source program @src@.
-- The result is a list of tuples consisting of a 'Position' and a 'Token'.
lexSource :: FilePath -> String -> Either Message [(Position, L.Token)]
lexSource fn src = runMsg $ L.lexSource fn src

-- |Parse a Curry 'Interface'
parseInterface :: FilePath -> String -> Either Message Interface
parseInterface fn src = runMsg $ P.parseInterface fn src

-- |Parse a Curry 'Module' header
parseHeader :: FilePath -> String -> Either Message Module
parseHeader fn src = runMsg $ P.parseHeader fn src

-- |Parse a Curry 'Module'
parseModule :: FilePath -> String -> Either Message Module
parseModule fn src = runMsg $ P.parseSource fn src

-- |Parse a 'Goal', i.e. an expression with (optional) local declarations
parseGoal :: String -> Either Message Goal
parseGoal src = runMsg $ P.parseGoal src
