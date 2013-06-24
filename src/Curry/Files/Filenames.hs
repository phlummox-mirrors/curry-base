{- |
    Module      :  $Header$
    Description :  File names for several intermediate file formats.
    Copyright   :  (c) Holger Siegel   2009
                       Björn Peemöller 2013
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    The functions in this module were collected from several compiler modules
    in order to provide a unique accessing point for this functionality.
-}
module Curry.Files.Filenames
  (
    -- * Special directories
    currySubdir

    -- * File name extensions
    -- ** Curry files
  , curryExt, lcurryExt, icurryExt

    -- ** FlatCurry files
  , flatExt, extFlatExt, flatIntExt, xmlExt, xmlSuffix

    -- ** AbstractCurry files
  , acyExt, uacyExt

    -- ** Source and object files
  , sourceRepExt, sourceExts, moduleExts

    -- * Functions for computing file names
  , interfName, flatName, extFlatName, flatIntName, xmlName
  , acyName, uacyName
  , sourceRepName
  ) where

import System.FilePath (takeBaseName, replaceBaseName, replaceExtension)

-- |The standard hidden subdirectory for curry files
currySubdir :: String
currySubdir = ".curry"

-- |Filename extension for non-literate curry files
curryExt :: String
curryExt = ".curry"

-- |Filename extension for literate curry files
lcurryExt :: String
lcurryExt = ".lcurry"

-- |Filename extension for curry interface files
icurryExt :: String
icurryExt = ".icurry"

-- |Filename extension for curry source files.
--
-- /Note:/ The order of the extensions defines the order in which source files
-- should be searched for, i.e. given a module name @M@, the search order
-- should be the following:
--
-- 1. @M.curry@
-- 2. @M.lcurry@
--
sourceExts :: [String]
sourceExts = [curryExt, lcurryExt]

-- |Filename extension for curry module files
-- TODO: Is the order correct?
moduleExts :: [String]
moduleExts = sourceExts ++ [icurryExt]

-- |Filename extension for flat-curry files
flatExt :: String
flatExt = ".fcy"

-- |Filename extension for extended-flat-curry files
extFlatExt :: String
extFlatExt = ".efc"

-- |Filename extension for extended-flat-curry interface files
flatIntExt :: String
flatIntExt = ".fint"

-- |Filename extension for extended-flat-curry xml files
xmlExt :: String
xmlExt = ".xml"

-- |Basename suffix for extended-flat-curry xml files
xmlSuffix :: String
xmlSuffix = "_flat"

-- |Filename extension for abstract-curry files
acyExt :: String
acyExt = ".acy"

-- |Filename extension for untyped-abstract-curry files
uacyExt :: String
uacyExt = ".uacy"

-- |Filename extension for curry source representation files
sourceRepExt :: String
sourceRepExt = ".cy"

-- ---------------------------------------------------------------------------
-- Computation of file names for a given source file
-- ---------------------------------------------------------------------------

-- |Compute the filename of the interface file for a source file
interfName :: FilePath -> FilePath
interfName = replaceExtensionWith icurryExt

-- |Compute the filename of the flat curry file for a source file
flatName :: FilePath -> FilePath
flatName = replaceExtensionWith flatExt

-- |Compute the filename of the extended flat curry file for a source file
extFlatName :: FilePath -> FilePath
extFlatName = replaceExtensionWith extFlatExt

-- |Compute the filename of the flat curry interface file for a source file
flatIntName :: FilePath -> FilePath
flatIntName = replaceExtensionWith flatIntExt

-- |Compute the filename of the flat curry xml file for a source file
xmlName :: FilePath -> FilePath
xmlName fn = replaceExtensionWith xmlExt
           $ replaceBaseName fn (takeBaseName fn ++ xmlSuffix)

-- |Compute the filename of the abstract curry file for a source file
acyName :: FilePath -> FilePath
acyName = replaceExtensionWith acyExt

-- |Compute the filename of the untyped abstract curry file for a source file
uacyName :: FilePath -> FilePath
uacyName = replaceExtensionWith uacyExt

-- |Compute the filename of the source representation file for a source file
sourceRepName :: FilePath -> FilePath
sourceRepName = replaceExtensionWith sourceRepExt

-- |Replace a filename extension with a new extension
replaceExtensionWith :: String -> FilePath -> FilePath
replaceExtensionWith = flip replaceExtension
