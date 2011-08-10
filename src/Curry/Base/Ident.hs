{- |
    Module      :  $Header$
    Description :  Identifiers
    Copyright   :  (c) 1999-2004, Wolfgang Lux
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  non-portable (DeriveDataTypeable)

    This module provides the implementation of identifiers and some
    utility functions for identifiers.

    Identifiers comprise the name of the denoted entity and an /id/,
    which can be used for renaming identifiers, e.g., in order to resolve
    name conflicts between identifiers from different scopes. An
    identifier with an /id/ @0@ is considered as not being renamed
    and, hence, its /id/ will not be shown.

    Qualified identifiers may optionally be prefixed by a module name.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Curry.Base.Ident
  ( -- * Identifiers
    -- ** Data types
    Ident (..), QualIdent (..), ModuleIdent (..), SrcRefOf (..)
    -- ** Functions
  , showIdent, qualName, moduleName, fromModuleName, mkIdent, identSupply, mkMIdent
  , renameIdent, unRenameIdent, isInfixOp, isQInfixOp
  , qualify, qualifyWith, qualQualify, isQualified, unqualify, qualUnqualify
  , localIdent, updIdentName, addPositionIdent, addPositionModuleIdent
  , addRef, addRefId, positionOfQualIdent, updQualIdent

    -- * Predefined simple identifiers
    -- ** Identifiers for modules
  , emptyMIdent, mainMIdent, preludeMIdent
    -- ** Identifiers for types
  , anonId, unitId, boolId, charId, intId, floatId, listId, ioId, successId
    -- ** Identifiers for constructors
  , trueId, falseId, nilId, consId, tupleId, isTupleId, tupleArity
    -- ** Identifiers for functions
  , mainId, minusId, fminusId

    -- * Predefined qualified identifiers
    -- ** Identifiers for types
  , qUnitId, qBoolId, qCharId, qIntId, qFloatId, qListId, qIOId, qSuccessId
    -- ** Identifiers for constructors
  , qTrueId, qFalseId, qNilId, qConsId, qTupleId, isQTupleId, qTupleArity

    -- * Extended functionality
    -- ** Function pattern
  , fpSelectorId, isFpSelectorId, isQualFpSelectorId
    -- ** Records
  , recSelectorId, qualRecSelectorId, recUpdateId, qualRecUpdateId
  , recordExtId, labelExtId, isRecordExtId, isLabelExtId, fromRecordExtId
  , fromLabelExtId, renameLabel, recordExt, labelExt, mkLabelIdent
  ) where

import Control.Monad (liftM)
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.Generics (Data(..), Typeable(..))
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, fromMaybe)

import Curry.Base.Position

-- TODO: Probably we should use \texttt{Integer} for the \emph{id}s.

-- | Simple identifiers
data Ident = Ident
  { positionOfIdent :: Position -- ^ Source code 'Position'
  , name            :: String   -- ^ name
  , uniqueId        :: Int      -- ^ unique number of the identifier
  } deriving (Read, Data, Typeable)

instance Eq Ident where
  Ident _ m i == Ident _ n j = (m, i) == (n, j)

instance Ord Ident where
  Ident _ m i `compare` Ident _ n j = (m, i) `compare` (n, j)

instance Show Ident where
  show = showIdent

instance SrcRefOf Ident where
  srcRefOf = srcRefOf . positionOfIdent

-- | Show function for an 'Ident'
showIdent :: Ident -> String
showIdent (Ident _ x 0) = x
showIdent (Ident _ x n) = x ++ '.' : show n


-- | Module identifiers
data ModuleIdent = ModuleIdent
  { positionOfModuleIdent :: Position -- ^ source code position
  , moduleQualifiers      :: [String] -- ^ hierarchical idenfiers
  } deriving (Read, Data, Typeable)

instance Eq ModuleIdent where
  (==) = (==) `on` moduleQualifiers

instance Ord ModuleIdent where
  compare = compare `on` moduleQualifiers

instance Show ModuleIdent where
  show = moduleName

instance SrcRefOf ModuleIdent where
  srcRefOf = srcRefOf . positionOfModuleIdent

-- | Retrieve the hierarchical name of a module
moduleName :: ModuleIdent -> String
moduleName = intercalate "." . moduleQualifiers

-- | Resemble the hierarchical module name from a 'String'
fromModuleName :: String -> ModuleIdent
fromModuleName = mkMIdent . splitQualifiers
  where splitQualifiers s = let (pref, rest) = break (== '.') s in
          pref : case rest of
            []     -> []
            (_:s') -> splitQualifiers s'


-- | Qualified identifiers
data QualIdent = QualIdent
  { qualidMod :: Maybe ModuleIdent -- ^ optional module identifier
  , qualidId  :: Ident             -- ^ identifier itself
  } deriving (Eq, Ord, Read, Data, Typeable)

instance SrcRefOf QualIdent where
  srcRefOf = srcRefOf . unqualify

instance Show QualIdent where
  show = qualName

-- | show function for qualified identifiers
qualName :: QualIdent -> String
qualName (QualIdent Nothing  x) = name x
qualName (QualIdent (Just m) x) = moduleName m ++ "." ++ name x


-- ---------------------------------------------------------------------------
-- Functions for working with identifiers
-- ---------------------------------------------------------------------------

-- | Add a 'Position' to an 'Ident'
addPositionIdent :: Position -> Ident -> Ident
addPositionIdent pos      (Ident NoPos x n) = Ident pos x n
addPositionIdent (AST sr) (Ident pos x n)   = Ident pos { astRef = sr } x n
addPositionIdent pos      (Ident _ x n)     = Ident pos x n

-- | Add a 'Position' to a 'ModuleIdent'
addPositionModuleIdent :: Position -> ModuleIdent -> ModuleIdent
addPositionModuleIdent pos mi = mi { positionOfModuleIdent = pos }

-- | Retrieve the 'Position' of a 'QualIdent'
positionOfQualIdent :: QualIdent -> Position
positionOfQualIdent = positionOfIdent . qualidId

-- | Construct an 'Ident' from a 'String'
mkIdent :: String -> Ident
mkIdent x = Ident NoPos x 0

-- |Infinite list of different 'Ident's
identSupply :: [Ident]
identSupply = [ mkNewIdent c i | i <- [0 ..] :: [Integer], c <- ['a'..'z'] ]
  where mkNewIdent c 0 = mkIdent [c]
        mkNewIdent c n = mkIdent $ c : show n

-- | Rename an 'Ident' by changing its unique number
renameIdent :: Ident -> Int -> Ident
renameIdent ident n = ident { uniqueId = n }

-- | Revert the renaming of an 'Ident' by resetting its unique number
unRenameIdent :: Ident -> Ident
unRenameIdent ident = renameIdent ident 0

-- | Change the name of an 'Ident' using a renaming function
updIdentName :: (String -> String) -> Ident -> Ident
updIdentName f (Ident p n i) =
  addPositionIdent p $ renameIdent (mkIdent (f n)) i

-- | Construct a 'ModuleIdent' from a list of 'String's forming the
--   the hierarchical module name.
mkMIdent :: [String] -> ModuleIdent
mkMIdent = ModuleIdent NoPos

-- | Check whether an 'Ident' identifies an infix operation
isInfixOp :: Ident -> Bool
isInfixOp (Ident _ ('<' : c : cs) _) =
  last (c : cs) /= '>' || not (isAlphaNum c) && c `notElem` "_(["
isInfixOp (Ident _ (c : _) _)    = not (isAlphaNum c) && c `notElem` "_(["
isInfixOp (Ident _ _ _)          = False -- error "Zero-length identifier"

-- | Check whether an 'QualIdent' identifies an infix operation
isQInfixOp :: QualIdent -> Bool
isQInfixOp = isInfixOp . qualidId

-- ---------------------------------------------------------------------------
-- The functions \texttt{qualify} and \texttt{qualifyWith} convert an
-- unqualified identifier into a qualified identifier (without and with a
-- given module prefix, respectively).
-- ---------------------------------------------------------------------------

-- | Convert an 'Ident' to a 'QualIdent'
qualify :: Ident -> QualIdent
qualify = QualIdent Nothing

-- | Convert an 'Ident' to a 'QualIdent' with a given 'ModuleIdent'
qualifyWith :: ModuleIdent -> Ident -> QualIdent
qualifyWith = QualIdent . Just

-- | Convert an 'QualIdent' to a new 'QualIdent' with a given 'ModuleIdent'.
--   If the original 'QualIdent' already contains an 'ModuleIdent' it
--   remains unchanged.
qualQualify :: ModuleIdent -> QualIdent -> QualIdent
qualQualify m (QualIdent Nothing x) = QualIdent (Just m) x
qualQualify _ x = x

-- | Check whether a 'QualIdent' contains a 'ModuleIdent'
isQualified :: QualIdent -> Bool
isQualified = isJust . qualidMod

-- | Remove the qualification of an 'QualIdent'
unqualify :: QualIdent -> Ident
unqualify = qualidId

-- | Remove the qualification with a specific 'ModuleIdent'. If the
--   original 'QualIdent' has no 'ModuleIdent' or a different one, it
--   remains unchanged.
qualUnqualify :: ModuleIdent -> QualIdent -> QualIdent
qualUnqualify _ qid@(QualIdent Nothing _) = qid
qualUnqualify m (QualIdent (Just m') x) = QualIdent m'' x
  where m'' | m == m'   = Nothing
            | otherwise = Just m'

-- | Extract the 'Ident' of an 'QualIdent' if it is local to the
--   'ModuleIdent', i.e. if the 'Ident' is either unqualified or qualified
--   with the given 'ModuleIdent'.
localIdent :: ModuleIdent -> QualIdent -> Maybe Ident
localIdent _ (QualIdent Nothing x) = Just x
localIdent m (QualIdent (Just m') x)
  | m == m'   = Just x
  | otherwise = Nothing

-- | Update a 'QualIdent' by applying functions to its components
updQualIdent :: (ModuleIdent -> ModuleIdent)
             -> (Ident -> Ident)
             -> QualIdent -> QualIdent
updQualIdent f g (QualIdent m x) = QualIdent (liftM f m) (g x)

-- | Add a 'SrcRef' to an 'Ident'
addRefId :: SrcRef -> Ident -> Ident
addRefId = addPositionIdent . AST

-- | Add a 'SrcRef' to a 'QualIdent'
addRef :: SrcRef -> QualIdent -> QualIdent
addRef = updQualIdent id . addRefId

-- ---------------------------------------------------------------------------
-- A few identifiers are predefined here.
-- ---------------------------------------------------------------------------

-- | 'ModuleIdent' for the empty module
emptyMIdent :: ModuleIdent
emptyMIdent = ModuleIdent NoPos []

-- | 'ModuleIdent' for the main module
mainMIdent :: ModuleIdent
mainMIdent = ModuleIdent NoPos ["main"]

-- | 'ModuleIdent' for the prelude
preludeMIdent :: ModuleIdent
preludeMIdent = ModuleIdent NoPos ["Prelude"]

-- | Construct a 'QualIdent' for an 'Ident' using the module prelude
qPreludeIdent :: Ident -> QualIdent
qPreludeIdent = qualifyWith preludeMIdent

-- | 'Ident' for anonymous variable
anonId :: Ident
anonId = mkIdent "_"

-- ---------------------------------------------------------------------------
-- Identifiers for types
-- ---------------------------------------------------------------------------

-- | 'Ident' for the type/value unit ('()')
unitId :: Ident
unitId = mkIdent "()"

-- | 'Ident' for the type 'Bool'
boolId :: Ident
boolId = mkIdent "Bool"

-- | 'Ident' for the type 'Char'
charId :: Ident
charId = mkIdent "Char"

-- | 'Ident' for the type 'Int'
intId :: Ident
intId = mkIdent "Int"

-- | 'Ident' for the type 'Float'
floatId :: Ident
floatId = mkIdent "Float"

-- | 'Ident' for the type '[]'
listId :: Ident
listId = mkIdent "[]"

-- | 'Ident' for the type 'IO'
ioId :: Ident
ioId = mkIdent "IO"

-- | 'Ident' for the type 'Success'
successId :: Ident
successId = mkIdent "Success"

-- ---------------------------------------------------------------------------
-- Identifiers for constructors
-- ---------------------------------------------------------------------------

-- | 'Ident' for the value 'True'
trueId :: Ident
trueId  = mkIdent "True"

-- | 'Ident' for the value 'False'
falseId :: Ident
falseId = mkIdent "False"

-- | 'Ident' for the value '[]'
nilId :: Ident
nilId   = mkIdent "[]"

-- | 'Ident' for the function ':'
consId :: Ident
consId  = mkIdent ":"

-- | Construct an 'Ident' for an n-ary tuple where n >= 2
tupleId :: Int -> Ident
tupleId n
  | n >= 2    = Ident NoPos ("(" ++ replicate (n - 1) ',' ++ ")") 0
  | otherwise = error $ "Curry.Base.Ident.tupleId: " ++ show n

-- | Check whether an 'Ident' is an identifier for an tuple type
isTupleId :: Ident -> Bool
isTupleId x = n > 1 && x == tupleId n
  where n = length (name x) - 1

-- | Compute the arity of a tuple identifier
tupleArity :: Ident -> Int
tupleArity x
  | n > 1 && x == tupleId n = n
  | otherwise = error $ "Curry.Base.Ident.tupleArity: " ++ show x
  where n = length (name x) - 1

-- ---------------------------------------------------------------------------
-- Identifiers for functions
-- ---------------------------------------------------------------------------

-- | 'Ident' for the main function
mainId :: Ident
mainId   = mkIdent "main"

-- | 'Ident' for the minus function
minusId :: Ident
minusId  = mkIdent "-"

-- | 'Ident' for the -. function
fminusId :: Ident
fminusId = mkIdent "-."

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for types
-- ---------------------------------------------------------------------------

-- | 'QualIdent' for the type/value unit ('()')
qUnitId :: QualIdent
qUnitId = qualify unitId

-- | 'QualIdent' for the type 'Bool'
qBoolId :: QualIdent
qBoolId = qPreludeIdent boolId

-- | 'QualIdent' for the type 'Char'
qCharId :: QualIdent
qCharId = qPreludeIdent charId

-- | 'QualIdent' for the type 'Int'
qIntId :: QualIdent
qIntId = qPreludeIdent intId

-- | 'QualIdent' for the type 'Float'
qFloatId :: QualIdent
qFloatId = qPreludeIdent floatId

-- | 'QualIdent' for the type '[]'
qListId :: QualIdent
qListId = qualify listId

-- | 'QualIdent' for the type 'IO'
qIOId :: QualIdent
qIOId = qPreludeIdent ioId

-- | 'QualIdent' for the type 'Success'
qSuccessId :: QualIdent
qSuccessId = qPreludeIdent successId

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for constructors
-- ---------------------------------------------------------------------------

-- | 'QualIdent' for the constructor 'True'
qTrueId :: QualIdent
qTrueId = qPreludeIdent trueId

-- | 'QualIdent' for the constructor 'False'
qFalseId :: QualIdent
qFalseId = qPreludeIdent falseId

-- | 'QualIdent' for the constructor '[]'
qNilId :: QualIdent
qNilId = qualify nilId

-- | 'QualIdent' for the constructor ':'
qConsId :: QualIdent
qConsId = qualify consId

-- | 'QualIdent' for the type of n-ary tuples
qTupleId :: Int -> QualIdent
qTupleId = qualify . tupleId

-- | Check whether an 'QualIdent' is an identifier for an tuple type
isQTupleId :: QualIdent -> Bool
isQTupleId = isTupleId . unqualify

-- | Compute the arity of an qualified tuple identifier
qTupleArity :: QualIdent -> Int
qTupleArity = tupleArity . unqualify

-- ---------------------------------------------------------------------------
-- Micellaneous functions for generating and testing extended identifiers
-- ---------------------------------------------------------------------------

-- | Construct an 'Ident' for a function pattern
fpSelectorId :: Int -> Ident
fpSelectorId n = Ident NoPos (fpSelExt ++ show n) 0

-- | Check whether an 'Ident' is an identifier for a function pattern
isFpSelectorId :: Ident -> Bool
isFpSelectorId = (fpSelExt `isInfixOf`) . name
-- isFpSelectorId = any (fpSelExt `isPrefixOf`) . tails . name

-- | Check whether an 'QualIdent' is an identifier for a function pattern
isQualFpSelectorId :: QualIdent -> Bool
isQualFpSelectorId = isFpSelectorId . unqualify

-- | Construct an 'Ident' for a record selection pattern
recSelectorId :: QualIdent -- ^ identifier of the record
              -> Ident     -- ^ identifier of the label
              -> Ident
recSelectorId r l = mkIdent $ recSelExt ++ name (unqualify r) ++ "." ++ name l

-- | Construct a 'QualIdent' for a record selection pattern
qualRecSelectorId :: ModuleIdent -- ^ default module
                  -> QualIdent   -- ^ record identifier
                  -> Ident       -- ^ label identifier
                  -> QualIdent
qualRecSelectorId m r l = qualifyWith m' (recSelectorId r l)
  where m' = fromMaybe m (qualidMod r)

-- | Construct an 'Ident' for a record update pattern
recUpdateId :: QualIdent -- ^ record identifier
            -> Ident     -- ^ label identifier
            -> Ident
recUpdateId r l = mkIdent $ recUpdExt ++ name (unqualify r) ++ "." ++ name l

-- | Construct a 'QualIdent' for a record update pattern
qualRecUpdateId :: ModuleIdent -- ^ default module
                -> QualIdent   -- ^ record identifier
                -> Ident       -- ^ label identifier
                -> QualIdent
qualRecUpdateId m r l = qualifyWith m' (recUpdateId r l)
  where m' = fromMaybe m (qualidMod r)

-- | Construct an 'Ident' for a record
recordExtId :: Ident -> Ident
recordExtId r = mkIdent $ recordExt ++ name r

-- | Construct an 'Ident' for a record label
labelExtId :: Ident -> Ident
labelExtId l = mkIdent $ labelExt ++ name l

-- | Retrieve the 'Ident' from a record identifier
fromRecordExtId :: Ident -> Ident
fromRecordExtId r
  | p == recordExt = mkIdent r'
  | otherwise      = r
 where (p, r') = splitAt (length recordExt) (name r)

-- | Retrieve the 'Ident' from a record label identifier
fromLabelExtId :: Ident -> Ident
fromLabelExtId l
  | p == labelExt = mkIdent l'
  | otherwise     = l
 where (p, l') = splitAt (length labelExt) (name l)

-- | Check whether an 'Ident' is an identifier for a record
isRecordExtId :: Ident -> Bool
isRecordExtId r = recordExt `isPrefixOf` name r

-- | Check whether an 'Ident' is an identifier for a record label
isLabelExtId :: Ident -> Bool
isLabelExtId l = labelExt `isPrefixOf` name l

-- | Construct an 'Ident' for a record label
mkLabelIdent :: String -> Ident
mkLabelIdent c = renameIdent (mkIdent c) (-1)

-- | Rename an 'Ident' for a record label
renameLabel :: Ident -> Ident
renameLabel l = renameIdent l (-1)

-- | Annotation for function pattern identifiers
fpSelExt :: String
fpSelExt = "_#selFP"

-- | Annotation for record selection identifiers
recSelExt :: String
recSelExt = "_#selR@"

-- | Annotation for record update identifiers
recUpdExt :: String
recUpdExt = "_#updR@"

-- | Annotation for record identifiers
recordExt :: String
recordExt = "_#Rec:"

-- | Annotation for record label identifiers
labelExt :: String
labelExt = "_#Lab:"
