{- |Since version 0.7 of the language report, Curry accepts literate
    source programs. In a literate source, all program lines must begin
    with a greater sign in the first column. All other lines are assumed
    to be documentation. In order to avoid some common errors with
    literate programs, Curry requires at least one program line to be
    present in the file. In addition, every block of program code must be
    preceded by a blank line and followed by a blank line.

    This module has been rewritten by Holger Siegel in 2009.

    (c) Holger Siegel, 2009.
-}

module Curry.Files.Unlit (unlit, isLiterate) where

import Control.Monad (when, zipWithM)
import Data.Char (isSpace)

import Curry.Base.Position
import Curry.Base.MessageMonad
import Curry.Files.Filenames (lcurryExt)
import Curry.Files.PathUtils (takeExtension)

-- |Data type representing different kind of lines in a literate source
data Line
  = Program !Int String -- ^ program line with a line number and content
  | Blank               -- ^ blank line
  | Comment             -- ^ comment line

isLiterate :: FilePath -> Bool
isLiterate = (== lcurryExt) . takeExtension

{- |Process a curry program into error messages (if any) and the
    corresponding non-literate program.
-}
unlit :: FilePath -> String -> MsgMonad String
unlit fn cy
  | isLiterate fn = do
      ls <- progLines fn $ zipWith classify [1 .. ] $ lines cy
      when (all null ls) $ failWith (fn ++ ": no code in literate script")
      return (unlines ls)
  | otherwise     = return cy

-- |Classify a line
classify :: Int -> String -> Line
classify l ('>' : cs) = Program l cs
classify _ cs | all isSpace cs = Blank
              | otherwise      = Comment

{- |Check that each program line is not adjacent to a comment line and there
    is at least one program line.
-}
progLines :: FilePath -> [Line] -> MsgMonad [String]
progLines fn cs = zipWithM checkAdjacency (Blank : cs) cs where
  checkAdjacency :: Line -> Line -> MsgMonad String
  checkAdjacency (Program p _) Comment       = message fn p "followed"
  checkAdjacency Comment       (Program p _) = message fn p "preceded"
  checkAdjacency _             (Program _ s) = return s
  checkAdjacency _             _             = return ""

-- |Compute an appropiate error message
message :: String -> Int -> String -> MsgMonad a
message f l cause = failWithAt (Position f l 1 noRef) msg
  where msg = "When reading literate source: Program line is "
              ++ cause ++ " by comment line."
