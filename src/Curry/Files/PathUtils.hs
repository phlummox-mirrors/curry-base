{- |
    Module      :  $Header$
    Description :  Utility functions for reading and writing files
    Copyright   :  (c) 1999-2003, Wolfgang Lux
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable
-}
module Curry.Files.PathUtils
  ( -- * Re-exports from 'System.FilePath'
    takeBaseName, dropExtension, takeExtension, takeFileName

  , moduleNameToFile, fileNameToModule, isCurryFilePath

    -- * Retrieving curry files
  , lookupCurryFileIn        , lookupCurryFile
  , lookupCurryModuleIn      , lookupCurryModule
  , lookupCurryInterfaceIn   , lookupCurryInterface
  , lookupFileIn             , lookupFile

    -- * Reading and writing modules from files
  , writeModule, readModule, maybeReadModule
  , doesModuleExist, getModuleModTime, tryGetModuleModTime
  ) where

import qualified Control.Exception as C (IOException, catch)
import Control.Monad (liftM)
import System.FilePath
import System.Directory
import System.Time (ClockTime)

import Curry.Base.Ident
import Curry.Files.Filenames

moduleNameToFile :: ModuleIdent -> FilePath
moduleNameToFile = foldr1 (</>) . moduleQualifiers

fileNameToModule :: FilePath -> ModuleIdent
fileNameToModule = mkMIdent . splitDirectories . dropExtension . dropDrive

-- ---------------------------------------------------------------------------
-- Searching for files
-- ---------------------------------------------------------------------------

isCurryFilePath :: String -> Bool
isCurryFilePath str = isValid str && takeExtension str `elem` ("" : moduleExts)

-- combine lifted into nested monads
combineM :: (Monad m, Monad m1)
         => (m (m1 (FilePath, FilePath)))
         -> (m (m1 FilePath))
combineM = liftM (liftM $ uncurry combine)

-- | Search in the given list of paths for the given 'FilePath' and eventually
-- return the file name and the path the file was found in.
--
-- - If the file name already contains a directory, then the paths to search
--   in are ignored.
-- - If the file name has no extension, then a source file extension is
--   assumed.
lookupCurryFileIn :: [FilePath] -> FilePath -> IO (Maybe (FilePath,FilePath))
lookupCurryFileIn paths fn = lookupFileIn ("" : paths') exts fn where
  paths' | pathSeparator `elem` fn = []
         | otherwise               = paths
  exts   | null fnExt = sourceExts
         | otherwise  = [fnExt]
  fnExt = takeExtension fn

-- | Same as 'lookupCurryFileIn', but eventually returns the complete
-- 'FilePath'.
lookupCurryFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
lookupCurryFile paths fn = combineM $ lookupCurryFileIn paths fn

-- |Search for a given curry module in the given source file and
-- library paths. Note that the current directory is always searched first.
-- Returns the path as well as the file name without the path.
lookupCurryModuleIn :: [FilePath]          -- ^ list of paths to source files
                    -> [FilePath]          -- ^ list of paths to library files
                    -> ModuleIdent         -- ^ module identifier
                    -> IO (Maybe (FilePath, FilePath))
lookupCurryModuleIn paths libPaths m =
  lookupFileIn ("" : paths ++ libPaths) moduleExts $ moduleNameToFile m

-- | Same as 'lookupCurryModuleIn', but eventually returns the complete
-- 'FilePath'.
lookupCurryModule :: [FilePath] -> [FilePath] -> ModuleIdent
                  -> IO (Maybe FilePath)
lookupCurryModule paths libPaths m = combineM
                                   $ lookupCurryModuleIn paths libPaths m

-- |Search for an interface file in the import search path using the
-- interface extension 'flatIntExt'. Note that the current directory is
-- always searched first.
lookupCurryInterfaceIn :: [FilePath]          -- ^ list of paths to search in
                       -> ModuleIdent         -- ^ module identifier
                       -> IO (Maybe (FilePath, FilePath))
lookupCurryInterfaceIn paths m =
  lookupFileIn ("" : paths) [flatIntExt] $ moduleNameToFile m

-- |Search for an interface file in the import search path using the
-- interface extension 'flatIntExt'. Note that the current directory is
-- always searched first.
lookupCurryInterface :: [FilePath]          -- ^ list of paths to search in
                     -> ModuleIdent         -- ^ module identifier
                     -> IO (Maybe FilePath) -- ^ the file path if found
lookupCurryInterface paths m = combineM $ lookupCurryInterfaceIn paths m

lookupFileIn :: [FilePath]          -- ^ Directories to search in
             -> [String]            -- ^ Accepted file extensions
             -> FilePath            -- ^ Initial file name
             -> IO (Maybe (FilePath, FilePath))
lookupFileIn paths exts file = lookup' files
  where
  paths'    = concatMap (\p -> [p, ensureCurrySubdir p]) paths
  baseNames = map (replaceExtension file) exts
  files     = [ (p, f) | p <- paths', f <- baseNames ]

  lookup' []             = return Nothing
  lookup' ((p, f) : pfs) = do
    exists <- doesFileExistIn p f
    if exists then return (Just (p, f)) else lookup' pfs

-- |Search in the given directories for the file with the specified file
--  extensions and eventually return its 'FilePath'
lookupFile :: [FilePath]          -- ^ Directories to search in
           -> [String]            -- ^ Accepted file extensions
           -> FilePath            -- ^ Initial file name
           -> IO (Maybe FilePath) -- ^ 'FilePath' of the file if found
lookupFile paths exts file = combineM $ lookupFileIn paths exts file

doesFileExistIn :: FilePath -> FilePath -> IO Bool
doesFileExistIn path file = doesFileExist $ path </> file

-- ---------------------------------------------------------------------------
-- Reading and writing files
-- ---------------------------------------------------------------------------

-- | Write the content to a file in the given directory or in the
-- 'currySubdir' sub-directory if the first parameter is set to 'True'.
writeModule :: Bool     -- ^ should the 'currySubdir' be included in the path?
            -> FilePath -- ^ original path
            -> String   -- ^ file content
            -> IO ()
writeModule inSubdir filename contents = do
  let fn = if inSubdir then ensureCurrySubdir filename else filename
  createDirectoryIfMissing True $ takeDirectory fn
  writeFile fn contents

-- | Read the content from a file in the given directory or in the
-- 'currySubdir' sub-directory of the given sub-directory.
readModule :: FilePath -> IO String
readModule = onExistingFileDo readFile

-- | Tries to read the specified module and returns either 'Just String' if
-- reading was successful or 'Nothing' otherwise.
maybeReadModule :: FilePath -> IO (Maybe String)
maybeReadModule f = (Just `liftM` readModule f) `C.catch` ignoreIOException

{- | Check whether a module exists either in the given directory or in the
     'currySubdir'.
-}
doesModuleExist :: FilePath -> IO Bool
doesModuleExist = onExistingFileDo doesFileExist

-- | Get the modification time of a file, may raise an IO error
getModuleModTime :: FilePath -> IO ClockTime
getModuleModTime = onExistingFileDo getModificationTime


tryGetModuleModTime :: FilePath -> IO (Maybe ClockTime)
tryGetModuleModTime f = (Just `liftM` getModuleModTime f)
                        `C.catch` ignoreIOException

-- Helper functions

ignoreIOException :: C.IOException -> IO (Maybe a)
ignoreIOException _ = return Nothing

{- | Ensure that the 'currySubdir' is the last component of the
     directory structure of the given 'FilePath'. If the 'FilePath' already
     contains the 'currySubdir' it remains unchanged.
-}
ensureCurrySubdir :: FilePath -> FilePath
ensureCurrySubdir = ensureSubdir currySubdir

-- | Ensure that the given sub-directory is the last component of the
-- directory structure of the given 'FilePath'. If the 'FilePath' already
-- contains the sub-directory it remains unchanged.
ensureSubdir :: String   -- ^ sub-directory to add
             -> FilePath -- ^ original 'FilePath'
             -> FilePath -- ^ original 'FilePath'
ensureSubdir subdir file
  = replaceDirectory file
  $ addSub (splitDirectories $ takeDirectory file) subdir
  where
    addSub :: [String] -> String -> String
    addSub [] sub      = sub
    addSub ds sub
      | last ds == sub = joinPath ds
      | otherwise      = joinPath ds </> sub

-- | Perform an action on a file either in the given directory or else in the
-- 'currySubdir' sub-directory.
onExistingFileDo :: (FilePath -> IO a) -> FilePath -> IO a
onExistingFileDo act filename = do
  exists <- doesFileExist filename
  if exists then act filename
            else act $ ensureCurrySubdir filename
