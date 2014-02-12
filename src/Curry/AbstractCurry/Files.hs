{- |
    Module      :  $Header$
    Description :  Library to support meta-programming in Curry
    Copyright   :  (c) Michael Hanus, April 2004
                       Martin Engelke, July 2005
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This library contains I/O actions to read Curry programs
    and transform them into this abstract representation as well as
    write them to a file.

    /Assumption:/ An AbstractCurry program @Prog@ is stored in a file with
    the file extension @acy@, i.e. in a file @Prog.acy@.
-}
module Curry.AbstractCurry.Files
  ( readCurry, writeCurry, showCurry
  ) where

import qualified Control.Exception        as C (catch)
import           Control.Monad                 (liftM)
import           Data.List                     (intercalate)

import           Curry.Files.PathUtils         (writeModule, readModule)

import           Curry.AbstractCurry.Type

-- ---------------------------------------------------------------------------
-- Reading and writing AbstractCurry terms
-- ---------------------------------------------------------------------------

-- |Read an AbstractCurry file and return the corresponding AbstractCurry
--  program term of type 'CurryProg'
readCurry :: FilePath -> IO (Maybe CurryProg)
readCurry = liftM (fmap read) . readModule

-- |Write an AbstractCurry program term into a file.
--
--  If the flag is set, the file will be written into the hidden @.curry@
--  sub-directory.
writeCurry :: Bool -> FilePath -> CurryProg -> IO ()
writeCurry inHiddenSubdir filename prog
  = C.catch (writeModule inHiddenSubdir filename $ showCurry prog) ioError

-- |Show an AbstractCurry program in a nicer way
showCurry :: CurryProg -> String
showCurry (CurryProg mname imps types funcs ops)
  =  "CurryProg " ++ show mname ++ "\n"
  ++ " "  ++ show imps ++ "\n"
  ++ " [" ++ intercalate ",\n  " (map show types) ++ "]\n"
  ++ " [" ++ intercalate ",\n  " (map show funcs) ++ "]\n"
  ++ " "  ++ show ops ++ "\n"
