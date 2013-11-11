{- |
    Module      :  $Header$
    Description :  Library to support meta-programming in Curry
    Copyright   :  Michael Hanus  , April 2004
                   Martin Engelke , July 2005
                   Björn Peemöller, November 2013
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This library contains a definition for representing Curry programs
    in Haskell by the type 'CurryProg' and I/O actions to read Curry programs
    and transform them into this abstract representation as well as
    write them to a file.

    Note that this defines a slightly new format for AbstractCurry
    in comparison to the first proposal of 2003.

    /Assumption:/ An AbstractCurry program @Prog@ is stored in a file with
    the file extension @acy@, i.e. in a file @Prog.acy@.
-}
module Curry.AbstractCurry
  ( module Curry.AbstractCurry.Type
  , module Curry.AbstractCurry.Files
  ) where

import Curry.AbstractCurry.Type
import Curry.AbstractCurry.Files
