{- |
    Module      :  $Header$
    Description :  Interface for reading and manipulating Curry source code
    Copyright   :  (c) Holger Siegel 2009
                       Bjorrn Peemoeller 2011
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}
module Curry.Syntax
  ( module Curry.Syntax.Type
  , module Curry.Syntax.Utils
  , Lexer.Token (..), Lexer.Category (..), Lexer.Attributes (..)
  , lexFile
  , parseHeader
  , parseModule
  , ppModule, ppIDecl
  , showModule
  ) where

import           Curry.Base.Position               (Position)
import           Curry.Base.MessageMonad           (MsgMonad)
import           Curry.Files.Unlit                 (unlit)

import qualified Curry.Syntax.Lexer      as Lexer
import qualified Curry.Syntax.Parser     as Parser (parseHeader, parseSource)
import           Curry.Syntax.Pretty               (ppModule, ppIDecl)
import           Curry.Syntax.ShowModule           (showModule)
import           Curry.Syntax.Type
import           Curry.Syntax.Utils

-- |Return the result of a lexical analysis of the source program @src@.
--
--  The result is a list of tuples consisting of a 'Position' and a 'Token'.
lexFile :: FilePath -> String -> MsgMonad [(Position, Lexer.Token)]
lexFile fn src = unlit fn src >>= Lexer.lexFile fn

-- |Parse a Curry header
parseHeader :: FilePath -> String -> MsgMonad Module
parseHeader fn src = unlit fn src >>= Parser.parseHeader fn

-- |Parse a Curry module
parseModule :: Bool -> FilePath -> String -> MsgMonad Module
parseModule likeFlat fn src = unlit fn src >>= Parser.parseSource likeFlat fn
