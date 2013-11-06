{- |
    Module      :  $Header$
    Description :  Identifiers
    Copyright   :  (c) 1999 - 2004, Wolfgang Lux
                       2011 - 2013, Björn Peemöller
                       2013       , Matthias Böhm
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
  ( -- * Module identifiers
    ModuleIdent (..), mkMIdent, moduleName, escModuleName
  , fromModuleName, isValidModuleName, addPositionModuleIdent

    -- * Local identifiers
  , Ident (..), mkIdent, showIdent, escName, identSupply
  , globalScope, hasGlobalScope, isRenamed, renameIdent, unRenameIdent
  , updIdentName, addPositionIdent, addRefId, isInfixOp

    -- * Qualified identifiers
  , QualIdent (..), qualName, escQualName, qidPosition, isQInfixOp, qualify
  , qualifyWith, qualQualify, qualifyLike, isQualified, unqualify, qualUnqualify
  , localIdent, updQualIdent, addRef

    -- * Predefined simple identifiers
    -- ** Identifiers for modules
  , emptyMIdent, mainMIdent, preludeMIdent, tcPreludeMIdent
    -- ** Identifiers for types
  , unitId, boolId, charId, intId, floatId, listId, ioId, successId, arrowId
    -- ** Identifiers for constructors
  , trueId, falseId, nilId, consId, tupleId, isTupleId, tupleArity
    -- ** Identifiers for values
  , mainId, minusId, fminusId, anonId, isAnonId

    -- * Predefined qualified identifiers
    -- ** Identifiers for types
  , qUnitId, qBoolId, qCharId, qIntId, qFloatId, qListId, qIOId, qSuccessId
  , qArrowId, qUnitIdP, qListIdP, qArrowIdP, qTupleIdP
    -- ** Helper functions for qualified type constructors
  , hasSpecialSyntax
    -- ** Identifiers for constructors
  , qTrueId, qFalseId, qNilId, qConsId, qTupleId, isQTupleId, qTupleArity
    

    -- * Extended functionality
    -- ** Function pattern
  , fpSelectorId, isFpSelectorId, isQualFpSelectorId
    -- ** Records
  , recSelectorId, qualRecSelectorId, recUpdateId, qualRecUpdateId
  , recordExtId, labelExtId, isRecordExtId, isLabelExtId, fromRecordExtId
  , fromLabelExtId, renameLabel, recordExt, labelExt, mkLabelIdent
    -- ** Constructed identifiers
  , identPrefix, Curry.Base.Ident.sep, extractOrigName
  ) where

import Data.Char           (isAlpha, isAlphaNum, isSpace)
import Data.Function       (on)
import Data.Generics       (Data(..), Typeable(..))
import Data.List           (intercalate, isInfixOf, isPrefixOf)
import Data.Maybe          (isJust, fromMaybe)

import Curry.Base.Position
import Curry.Base.Pretty

-- ---------------------------------------------------------------------------
-- Module identifier
-- ---------------------------------------------------------------------------

-- | Module identifier
data ModuleIdent = ModuleIdent
  { midPosition   :: Position -- ^ source code 'Position'
  , midQualifiers :: [String] -- ^ hierarchical idenfiers
  } deriving (Data, Typeable)

instance Eq ModuleIdent where
  (==) = (==) `on` midQualifiers

instance Ord ModuleIdent where
  compare = compare `on` midQualifiers

instance Read ModuleIdent where
  readsPrec _ r = [ (mkMIdent is, s) | (is, s) <- readsIdents r]
    where
    readsIdents s =    [ ([i] , t) | (i, t    ) <- readsIdent  s ]
                    ++ [ (i:is, u) | (i, '.':t) <- readsIdent  s
                                   , (is, u   ) <- readsIdents t ]
    readsIdent s | null i    = []
                 | otherwise = [(i, t)]
      where (i, t) = span (\c -> not (isSpace c || c == '.')) s

instance Show ModuleIdent where
  show = moduleName

instance HasPosition ModuleIdent where
  getPosition = midPosition
  setPosition = addPositionModuleIdent

instance Pretty ModuleIdent where
  pPrint = hcat . punctuate dot . map text . midQualifiers

instance SrcRefOf ModuleIdent where
  srcRefOf = srcRefOf . getPosition

-- |Construct a 'ModuleIdent' from a list of 'String's forming the
--  the hierarchical module name.
mkMIdent :: [String] -> ModuleIdent
mkMIdent = ModuleIdent NoPos

-- |Retrieve the hierarchical name of a module
moduleName :: ModuleIdent -> String
moduleName = intercalate "." . midQualifiers

-- |Show the name of an 'ModuleIdent' escaped by ticks
escModuleName :: ModuleIdent -> String
escModuleName m = '`' : moduleName m ++ "'"

-- |Add a source code 'Position' to a 'ModuleIdent'
addPositionModuleIdent :: Position -> ModuleIdent -> ModuleIdent
addPositionModuleIdent pos mi = mi { midPosition = pos }

-- |Check whether a 'String' is a valid module name.
--
-- Valid module names must satisfy the following conditions:
--
--  * The name must not be empty
--  * The name must consist of one or more single identifiers,
--    seperated by dots
--  * Each single identifier must be non-empty, start with a letter and
--    consist of letter, digits, single quotes or underscores only
isValidModuleName :: String -> Bool
isValidModuleName [] = False -- Module names may not be empty
isValidModuleName qs = all isModuleIdentifier $ splitIdentifiers qs
  where
  -- components of a module identifier may not be null
  isModuleIdentifier []     = False
  -- components of a module identifier must start with a letter and consist
  -- of letter, digits, underscores or single quotes
  isModuleIdentifier (c:cs) = isAlpha c && all isIdent cs
  isIdent c                 = isAlphaNum c || c `elem` "'_"

-- |Resemble the hierarchical module name from a 'String' by splitting
-- the 'String' at inner dots.
--
-- /Note:/ This function does not check the 'String' to be a valid module
-- identifier, use isValidModuleName for this purpose.
fromModuleName :: String -> ModuleIdent
fromModuleName = mkMIdent . splitIdentifiers

-- Auxiliary function to split a hierarchical module identifier at the dots
splitIdentifiers :: String -> [String]
splitIdentifiers s = let (pref, rest) = break (== '.') s in
  pref : case rest of
    []     -> []
    (_:s') -> splitIdentifiers s'

-- ---------------------------------------------------------------------------
-- Simple identifier
-- ---------------------------------------------------------------------------

-- |Simple identifier
data Ident = Ident
  { idPosition :: Position -- ^ Source code 'Position'
  , idName     :: String   -- ^ Name of the identifier
  , idUnique   :: Integer  -- ^ Unique number of the identifier
  } deriving (Data, Typeable)

instance Eq Ident where
  Ident _ m i == Ident _ n j = (m, i) == (n, j)

instance Ord Ident where
  Ident _ m i `compare` Ident _ n j = (m, i) `compare` (n, j)

instance Read Ident where
  readsPrec _ r =   [ (mkIdent i, s) | (i, s    ) <- readsIdent r ]
                 ++ [ (renameIdent (mkIdent i) n, t)
                    | (i, '.':s) <- readsIdent  r
                    , (n, t    ) <- reads s
                    ]
    where readsIdent s | null i    = []
                       | otherwise = [(i, t)]
            where (i, t) = span (\c -> not (isSpace c || c == '.')) s

instance Show Ident where
  show = showIdent

instance HasPosition Ident where
  getPosition = idPosition
  setPosition = addPositionIdent

instance Pretty Ident where
  pPrint (Ident _ x n) | n == globalScope = text x
                       | otherwise        = text x <> dot <> integer n

instance SrcRefOf Ident where
  srcRefOf = srcRefOf . getPosition

-- |Global scope for renaming
globalScope :: Integer
globalScope = 0

-- |Construct an 'Ident' from a 'String'
mkIdent :: String -> Ident
mkIdent x = Ident NoPos x globalScope

-- |Infinite list of different 'Ident's
identSupply :: [Ident]
identSupply = drop 1 $ [ mkNewIdent c i | i <- [0 ..] :: [Integer], c <- ['a'..'z'] ]
  where mkNewIdent c 0 = mkIdent [c]
        mkNewIdent c n = mkIdent $ c : show n

-- |Show function for an 'Ident'
showIdent :: Ident -> String
showIdent (Ident _ x n) | n == globalScope = x
                        | otherwise        = x ++ '.' : show n

-- |Show the name of an 'Ident' escaped by ticks
escName :: Ident -> String
escName i = '`' : idName i ++ "'"

-- |Has the identifier global scope?
hasGlobalScope :: Ident -> Bool
hasGlobalScope = (== globalScope) . idUnique

-- |Is the 'Ident' renamed?
isRenamed :: Ident -> Bool
isRenamed = (/= globalScope) . idUnique

-- |Rename an 'Ident' by changing its unique number
renameIdent :: Ident -> Integer -> Ident
renameIdent ident n = ident { idUnique = n }

-- |Revert the renaming of an 'Ident' by resetting its unique number
unRenameIdent :: Ident -> Ident
unRenameIdent ident = renameIdent ident globalScope

-- |Change the name of an 'Ident' using a renaming function
updIdentName :: (String -> String) -> Ident -> Ident
updIdentName f (Ident p n i) = Ident p (f n) i

-- |Add a 'Position' to an 'Ident'
addPositionIdent :: Position -> Ident -> Ident
addPositionIdent pos      (Ident NoPos x n) = Ident pos x n
addPositionIdent (AST sr) (Ident pos   x n) = Ident pos { astRef = sr } x n
addPositionIdent pos      (Ident _     x n) = Ident pos x n

-- |Add a 'SrcRef' to an 'Ident'
addRefId :: SrcRef -> Ident -> Ident
addRefId = addPositionIdent . AST

-- |Check whether an 'Ident' identifies an infix operation
isInfixOp :: Ident -> Bool
isInfixOp (Ident _ ('<' : c : cs) _) =
  last (c : cs) /= '>' || not (isAlphaNum c) && c `notElem` "_(["
-- constructed values start with '#', followed by an alpha-num character
isInfixOp (Ident _ (c1 : c2 : _) _) 
  | c1 == head identPrefix = not (isAlphaNum c2)
isInfixOp (Ident _ (c : _) _)    = not (isAlphaNum c) && c `notElem` "_(["
isInfixOp (Ident _ _ _)          = False -- error "Zero-length identifier"

-- ---------------------------------------------------------------------------
-- Qualified identifier
-- ---------------------------------------------------------------------------

-- |Qualified identifier
data QualIdent = QualIdent
  { qidModule :: Maybe ModuleIdent -- ^ optional module identifier
  , qidIdent  :: Ident             -- ^ identifier itself
  } deriving (Eq, Ord, Data, Typeable)

instance Read QualIdent where
  readsPrec _ r =    [ (QualIdent Nothing  i, s) | (i, s    ) <- reads r ]
                  ++ [ (QualIdent (Just m) i, t) | (m, '.':s) <- reads r
                                                 , (i, t    ) <- reads s ]

instance Show QualIdent where
  show (QualIdent Nothing  x) = showIdent x
  show (QualIdent (Just m) x) = moduleName m ++ "." ++ showIdent x

instance HasPosition QualIdent where
  getPosition     = getPosition . qidIdent
  setPosition p q = q { qidIdent = setPosition p $ qidIdent q }

instance Pretty QualIdent where
  pPrint = text . show

instance SrcRefOf QualIdent where
  srcRefOf = srcRefOf . unqualify

-- |show function for qualified identifiers
qualName :: QualIdent -> String
qualName (QualIdent Nothing  x) = idName x
qualName (QualIdent (Just m) x) = moduleName m ++ "." ++ idName x

-- |Show the name of an 'QualIdent' escaped by ticks
escQualName :: QualIdent -> String
escQualName qn = '`' : qualName qn ++ "'"

-- |Retrieve the 'Position' of a 'QualIdent'
qidPosition :: QualIdent -> Position
qidPosition = idPosition . qidIdent

-- |Check whether an 'QualIdent' identifies an infix operation
isQInfixOp :: QualIdent -> Bool
isQInfixOp = isInfixOp . qidIdent

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

-- |Qualify an 'Ident' with the 'ModuleIdent' of the given 'QualIdent',
-- if present.
qualifyLike :: QualIdent -> Ident -> QualIdent
qualifyLike (QualIdent Nothing  _) x = qualify x
qualifyLike (QualIdent (Just m) _) x = qualifyWith m x

-- | Check whether a 'QualIdent' contains a 'ModuleIdent'
isQualified :: QualIdent -> Bool
isQualified = isJust . qidModule

-- | Remove the qualification of an 'QualIdent'
unqualify :: QualIdent -> Ident
unqualify = qidIdent

-- | Remove the qualification with a specific 'ModuleIdent'. If the
--   original 'QualIdent' has no 'ModuleIdent' or a different one, it
--   remains unchanged.
qualUnqualify :: ModuleIdent -> QualIdent -> QualIdent
qualUnqualify _ qid@(QualIdent Nothing   _) = qid
qualUnqualify m     (QualIdent (Just m') x) = QualIdent m'' x
  where m'' | m == m'   = Nothing
            | otherwise = Just m'

-- | Extract the 'Ident' of an 'QualIdent' if it is local to the
--   'ModuleIdent', i.e. if the 'Ident' is either unqualified or qualified
--   with the given 'ModuleIdent'.
localIdent :: ModuleIdent -> QualIdent -> Maybe Ident
localIdent _ (QualIdent Nothing   x) = Just x
localIdent m (QualIdent (Just m') x)
  | m == m'   = Just x
  | otherwise = Nothing

-- | Update a 'QualIdent' by applying functions to its components
updQualIdent :: (ModuleIdent -> ModuleIdent) -> (Ident -> Ident)
             -> QualIdent -> QualIdent
updQualIdent f g (QualIdent m x) = QualIdent (fmap f m) (g x)

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

-- | 'ModuleIdent' for the Prelude
preludeMIdent :: ModuleIdent
preludeMIdent = ModuleIdent NoPos ["Prelude"]

-- | 'ModuleIdent' for type classes Prelude
tcPreludeMIdent :: ModuleIdent
tcPreludeMIdent = ModuleIdent NoPos ["Prelude"]

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

-- | Construct an 'Ident' for an n-ary tuple where n > 1
tupleId :: Int -> Ident
tupleId n
  | n > 1     = mkIdent $ '(' : replicate (n - 1) ',' ++ ")"
  | otherwise = error $ "Curry.Base.Ident.tupleId: wrong arity " ++ show n

-- | Check whether an 'Ident' is an identifier for an tuple type
isTupleId :: Ident -> Bool
isTupleId (Ident _ x _) = n > 1 && x == idName (tupleId n)
  where n = length x - 1

-- | Compute the arity of a tuple identifier
tupleArity :: Ident -> Int
tupleArity i@(Ident _ x _)
  | n > 1 && x == idName (tupleId n) = n
  | otherwise                        = error $
      "Curry.Base.Ident.tupleArity: no tuple identifier: " ++ show i
  where n = length x - 1

-- | 'Ident' for the arrow type
arrowId :: Ident
arrowId = mkIdent "(->)"

-- ---------------------------------------------------------------------------
-- Identifiers for constructors
-- ---------------------------------------------------------------------------

-- | 'Ident' for the value 'True'
trueId :: Ident
trueId = mkIdent "True"

-- | 'Ident' for the value 'False'
falseId :: Ident
falseId = mkIdent "False"

-- | 'Ident' for the value '[]'
nilId :: Ident
nilId = mkIdent "[]"

-- | 'Ident' for the function ':'
consId :: Ident
consId = mkIdent ":"

-- ---------------------------------------------------------------------------
-- Identifiers for values
-- ---------------------------------------------------------------------------

-- | 'Ident' for the main function
mainId :: Ident
mainId = mkIdent "main"

-- | 'Ident' for the minus function
minusId :: Ident
minusId = mkIdent "-"

-- | 'Ident' for the minus function for Floats
fminusId :: Ident
fminusId = mkIdent "-."

-- | 'Ident' for anonymous variable
anonId :: Ident
anonId = mkIdent "_"

-- |Check whether an 'Ident' represents an anonymous identifier ('anonId')
isAnonId :: Ident -> Bool
isAnonId = (== anonId) . unRenameIdent

-- ---------------------------------------------------------------------------
-- Qualified Identifiers for types
-- ---------------------------------------------------------------------------

-- | Construct a 'QualIdent' for an 'Ident' using the module prelude
qPreludeIdent :: Ident -> QualIdent
qPreludeIdent = qualifyWith preludeMIdent

-- | 'QualIdent' for the type/value unit ('()')
qUnitId :: QualIdent
qUnitId = qualify unitId

qUnitIdP :: QualIdent
qUnitIdP = qPreludeIdent unitId

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

qListIdP :: QualIdent
qListIdP = qPreludeIdent listId

-- | 'QualIdent' for the type 'IO'
qIOId :: QualIdent
qIOId = qPreludeIdent ioId

-- | 'QualIdent' for the type 'Success'
qSuccessId :: QualIdent
qSuccessId = qPreludeIdent successId

-- | 'QualIdent' for the arrow type
qArrowId :: QualIdent
qArrowId = qualify arrowId

qArrowIdP :: QualIdent
qArrowIdP = qPreludeIdent arrowId

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

qTupleIdP :: Int -> QualIdent
qTupleIdP = qPreludeIdent . tupleId

-- | Check whether an 'QualIdent' is an identifier for an tuple type
isQTupleId :: QualIdent -> Bool
isQTupleId = isTupleId . unqualify

-- | Compute the arity of an qualified tuple identifier
qTupleArity :: QualIdent -> Int
qTupleArity = tupleArity . unqualify

-- |returns whether the given type constructor has special syntax
hasSpecialSyntax :: QualIdent -> Bool
hasSpecialSyntax qid = qid == qUnitId || qid == qUnitIdP
    || qid == qListId || qid == qListIdP
    || isQTupleId qid 
    || qid == qArrowId || qid == qArrowIdP

-- ---------------------------------------------------------------------------
-- Micellaneous functions for generating and testing extended identifiers
-- ---------------------------------------------------------------------------

-- | Construct an 'Ident' for a functional pattern
fpSelectorId :: Int -> Ident
fpSelectorId n = mkIdent $ fpSelExt ++ show n

-- | Check whether an 'Ident' is an identifier for a functional pattern
isFpSelectorId :: Ident -> Bool
isFpSelectorId = (fpSelExt `isInfixOf`) . idName

-- | Check whether an 'QualIdent' is an identifier for a function pattern
isQualFpSelectorId :: QualIdent -> Bool
isQualFpSelectorId = isFpSelectorId . unqualify

-- | Construct an 'Ident' for a record selection pattern
recSelectorId :: QualIdent -- ^ identifier of the record
              -> Ident     -- ^ identifier of the label
              -> Ident
recSelectorId = mkRecordId recSelExt

-- | Construct a 'QualIdent' for a record selection pattern
qualRecSelectorId :: ModuleIdent -- ^ default module
                  -> QualIdent   -- ^ record identifier
                  -> Ident       -- ^ label identifier
                  -> QualIdent
qualRecSelectorId m r l = qualRecordId m r $ recSelectorId r l

-- | Construct an 'Ident' for a record update pattern
recUpdateId :: QualIdent -- ^ record identifier
            -> Ident     -- ^ label identifier
            -> Ident
recUpdateId = mkRecordId recUpdExt

-- | Construct a 'QualIdent' for a record update pattern
qualRecUpdateId :: ModuleIdent -- ^ default module
                -> QualIdent   -- ^ record identifier
                -> Ident       -- ^ label identifier
                -> QualIdent
qualRecUpdateId m r l = qualRecordId m r $ recUpdateId r l

-- Auxiliary function to construct a selector/update identifier
mkRecordId :: String -> QualIdent -> Ident -> Ident
mkRecordId ann r l = mkIdent $ concat
  [ann, idName (unqualify r), ".", idName l]

-- Auxiliary function to qualify a selector/update identifier
qualRecordId :: ModuleIdent -> QualIdent -> Ident -> QualIdent
qualRecordId m r = qualifyWith (fromMaybe m $ qidModule r)

-- | Construct an 'Ident' for a record
recordExtId :: Ident -> Ident
recordExtId r = mkIdent $ recordExt ++ idName r

-- | Construct an 'Ident' for a record label
labelExtId :: Ident -> Ident
labelExtId l = mkIdent $ labelExt ++ idName l

-- | Retrieve the 'Ident' from a record identifier
fromRecordExtId :: Ident -> Ident
fromRecordExtId r
  | p == recordExt = mkIdent r'
  | otherwise      = r
 where (p, r') = splitAt (length recordExt) (idName r)

-- | Retrieve the 'Ident' from a record label identifier
fromLabelExtId :: Ident -> Ident
fromLabelExtId l
  | p == labelExt = mkIdent l'
  | otherwise     = l
 where (p, l') = splitAt (length labelExt) (idName l)

-- | Check whether an 'Ident' is an identifier for a record
isRecordExtId :: Ident -> Bool
isRecordExtId = (recordExt `isPrefixOf`) . idName

-- | Check whether an 'Ident' is an identifier for a record label
isLabelExtId :: Ident -> Bool
isLabelExtId = (labelExt `isPrefixOf`) . idName

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

-- | the prefix for constructed identifiers
identPrefix :: String
identPrefix = "#"

-- |the seperator string used for constructing identifiers out of smaller
-- elements
sep :: String
sep = ":_"

-- |Extracts the original name of the given identifier. This is the string after
-- the last "sep". 
extractOrigName :: Ident -> Ident
extractOrigName = updIdentName extractOrigName' 

extractOrigName' :: String -> String
extractOrigName' s = last (split' s "" [])
  where 
  sep' = Curry.Base.Ident.sep
  lenSep = length sep'
  split' :: String -> String -> [String] -> [String]
  split' str acc1 acc2
    | sep' `isPrefixOf` str = split' (drop lenSep str) "" (reverse acc1 : acc2)
    | null str              = reverse (reverse acc1 : acc2)
    | otherwise             = split' (tail str) (head str : acc1) acc2
