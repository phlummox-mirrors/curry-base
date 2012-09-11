{- |
    Module      :  $Header$
    Description :  Handling of literate Curry files
    Copyright   :  (c) 2009 Holger Siegel
                       2012 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    Since version 0.7 of the language report, Curry accepts literate
    source programs. In a literate source, all program lines must begin
    with a greater sign in the first column. All other lines are assumed
    to be documentation. In order to avoid some common errors with
    literate programs, Curry requires at least one program line to be
    present in the file. In addition, every block of program code must be
    preceded by a blank line and followed by a blank line.
-}

module Curry.Files.Unlit (isLiterate, unlit) where

import Control.Monad    (when, zipWithM)
import Data.Char        (isSpace)

import Curry.Base.Position   (Position (..), first, noRef)
import Curry.Base.Message    (MessageM, failWithAt)
import Curry.Files.Filenames (lcurryExt)
import Curry.Files.PathUtils (takeExtension)

-- |Check whether a 'FilePath' represents a literate Curry module
isLiterate :: FilePath -> Bool
isLiterate = (== lcurryExt) . takeExtension

-- |Data type representing different kind of lines in a literate source
data Line
  = Program !Int String -- ^ program line with a line number and content
  | Blank               -- ^ blank line
  | Comment             -- ^ comment line

-- |Process a curry program into error messages (if any) and the
-- corresponding non-literate program.
unlit :: FilePath -> String -> MessageM String
unlit fn cy
  | isLiterate fn = do
      ls <- progLines fn $ zipWith classify [1 .. ] $ lines cy
      when (all null ls) $ failWithAt (first fn) "No code in literate script"
      return (unlines ls)
  | otherwise     = return cy

-- |Classify a line
classify :: Int -> String -> Line
classify l ('>' : cs) = Program l cs
classify _ cs | all isSpace cs = Blank
              | otherwise      = Comment

-- |Check that each program line is not adjacent to a comment line and there
-- is at least one program line.
progLines :: FilePath -> [Line] -> MessageM [String]
progLines fn cs = zipWithM checkAdjacency (Blank : cs) cs where
  checkAdjacency (Program p _) Comment       = report fn p "followed"
  checkAdjacency Comment       (Program p _) = report fn p "preceded"
  checkAdjacency _             (Program _ s) = return s
  checkAdjacency _             _             = return ""

-- |Compute an appropiate error message
report :: String -> Int -> String -> MessageM a
report f l cause = failWithAt (Position f l 1 noRef) msg
  where msg = concat [ "When reading literate source: "
                     , "Program line is " ++ cause ++ " by comment line."
                     ]
