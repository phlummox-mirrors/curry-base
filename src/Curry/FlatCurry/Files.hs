{- |
    Module      : $Header$
    Description : Functions for reading and writing FlatCurry files
    Copyright   : (c) Björn Peemöller 2014
    License     : OtherLicense

    Maintainer  : bjp@informatik.uni-kiel.de
    Stability   : experimental
    Portability : portable

    This module contains functions for reading and writing FlatCurry files.
-}

module Curry.FlatCurry.Files
  ( readFlatCurry, readFlatInterface, writeFlatCurry
  ) where

import Control.Monad         (liftM)
import Data.Char             (isSpace)
import Data.List             (intercalate)

import Curry.Files.Filenames (flatName, flatIntName)
import Curry.Files.PathUtils (writeModule, readModule)

import Curry.FlatCurry.Type  (Prog (..))


-- ---------------------------------------------------------------------------
-- Functions for reading and writing FlatCurry terms
-- ---------------------------------------------------------------------------

-- |Reads an ExtendedFlat file (extension ".efc") and eventually returns the
-- corresponding FlatCurry program term (type 'Prog').
readFlatCurry :: FilePath -> IO (Maybe Prog)
readFlatCurry = readFlat . flatName

-- |Reads a FlatInterface file (extension @.fint@) and returns the
-- corresponding term (type 'Prog') as a value of type 'Maybe'.
readFlatInterface :: FilePath -> IO (Maybe Prog)
readFlatInterface = readFlat . flatIntName

-- |Reads a Flat file and returns the corresponding term (type 'Prog') as
-- a value of type 'Maybe'.
-- Due to compatibility with PAKCS it is allowed to have a commentary
-- at the beginning of the file enclosed in {- ... -}.
readFlat :: FilePath -> IO (Maybe Prog)
readFlat = liftM (liftM (read . skipComment)) . readModule where
  skipComment s = case dropWhile isSpace s of
      '{' : '-' : s' -> dropComment s'
      s'             -> s'
  dropComment ('-' : '}' : xs) = xs
  dropComment (_ : xs)         = dropComment xs
  dropComment []               = []

-- |Writes a FlatCurry program term into a file.

-- If the flag is set, the file will be written into the hidden @.curry@
-- sub-directory.
writeFlatCurry :: Bool -> FilePath -> Prog -> IO ()
writeFlatCurry inHiddenSubdir filename
  = writeModule inHiddenSubdir filename . showFlatCurry

-- |Shows FlatCurry program in a nicer way.
showFlatCurry :: Prog -> String
showFlatCurry (Prog mname imps types funcs ops)
  =  "Prog " ++ show mname ++ "\n"
  ++ " "  ++ show imps ++ "\n"
  ++ " [" ++ intercalate ",\n  " (map show types) ++ "]\n"
  ++ " [" ++ intercalate ",\n  " (map show funcs) ++ "]\n"
  ++ " "  ++ show ops ++ "\n"
