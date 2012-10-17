{- |
    Module      :  $Header$
    Description :  Monads for message handling
    Copyright   :  2009 Holger Siegel
                   2012 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  non-portable (FlexibleContexts)

    The monads MessageM and MessageIO provide a common way to log warning
    messages and to stop execution when an error occurs. They may be used to
    integrate different compiler passes smoothly.
-}

{-# LANGUAGE FlexibleContexts #-}

module Curry.Base.Message
  ( Message (..), message, posMessage, showWarning, showError
  , ppMessage, ppWarning, ppError, ppMessages
  , MessageT, MessageM, MessageIO
  , runMessageT, failWith, failWithAt, warn, warnAt
  , runMsg, ok, toIO, fromIO
  ) where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Writer   (MonadWriter, WriterT, runWriterT, tell)
import Data.Maybe             (fromMaybe)
import Text.PrettyPrint

import Curry.Base.Position

-- ---------------------------------------------------------------------------
-- Message
-- ---------------------------------------------------------------------------

-- |Compiler message
data Message = Message
  { msgPos :: Maybe Position -- ^ optional source code position
  , msgTxt :: Doc            -- ^ the message itself
  }

instance Eq Message where
  Message p1 t1 == Message p2 t2 = (p1, show t1) == (p2, show t2)

instance Ord Message where
  Message p1 t1 `compare` Message p2 t2 = compare (p1, show t1) (p2, show t2)

instance Show Message where
  showsPrec _ = shows . ppMessage

instance HasPosition Message where
  getPosition     = fromMaybe NoPos . msgPos
  setPosition p m = m { msgPos = Just p }

instance Error Message where
  noMsg  = message (text "Failure!")
  strMsg = message . text

-- |Construct a 'Message' without a 'Position'
message :: Doc -> Message
message = Message Nothing

-- |Construct a message from an entity with a 'Position' and a text
posMessage :: HasPosition p => p -> Doc -> Message
posMessage p msg = Message (Just $ getPosition p) msg

-- |Show a 'Message' as a warning
showWarning :: Message -> String
showWarning = show . ppWarning

-- |Show a 'Message' as an error
showError :: Message -> String
showError = show . ppError

-- |Pretty print a 'Message'
ppMessage :: Message -> Doc
ppMessage = ppAs ""

-- |Pretty print a 'Message' as a warning
ppWarning :: Message -> Doc
ppWarning = ppAs "Warning"

-- |Pretty print a 'Message' as an error
ppError :: Message -> Doc
ppError = ppAs "Error"

-- |Pretty print a 'Message' with a given key
ppAs :: String -> Message -> Doc
ppAs key (Message mbPos txt) = posPP <+> keyPP $$ nest 4 txt
  where
  posPP = maybe empty ((<> colon) . ppPosition) mbPos
  keyPP = if null key then empty else text key <> colon

-- |Pretty print a list of 'Message's by vertical concatenation
ppMessages :: (Message -> Doc) -> [Message] -> Doc
ppMessages ppFun = foldr (\m ms -> text "" $+$ m $+$ ms) empty . map ppFun

-- ---------------------------------------------------------------------------
-- Message Monad
-- ---------------------------------------------------------------------------

-- |Message monad transformer enabling the reporting of 'Message's as
-- warnings and additionally a 'Message' as an error message.
type MessageT m = WriterT [Message] (ErrorT Message m)

-- |Evaluate the value of a 'MessageT m a'
runMessageT :: Monad m => MessageT m a -> m (Either Message (a, [Message]))
runMessageT = runErrorT . runWriterT

-- |Abort the computation with an error message
failWith :: MonadError Message m => String -> m b
failWith = throwError . message . text

-- |Abort the computation with an error message at a certain position
failWithAt :: MonadError Message m => Position -> String -> m a
failWithAt p msg = throwError $ posMessage p $ text msg

-- |Report a warning message
warn :: MonadWriter [Message] m => String -> m ()
warn s = tell [message $ text s]

-- |Report a warning message for a given position
warnAt :: MonadWriter [Message] m => Position -> String -> m ()
warnAt p s  = tell [posMessage p $ text s]

-- ---------------------------------------------------------------------------
-- Simple Message Monad
-- ---------------------------------------------------------------------------

-- |Simple message monad
type MessageM = MessageT Identity

-- |Evaluate the value of a 'MessageM a'
runMsg :: MessageM a -> (Either Message (a, [Message]))
runMsg = runIdentity . runMessageT

-- |Directly evaluate to the success value of a 'MessageM a'.
-- Errors are converted in a call to the 'error' function.
ok :: MessageM a -> a
ok = either (error . showError) fst . runMsg

-- ---------------------------------------------------------------------------
-- Message Monad with IO
-- ---------------------------------------------------------------------------

-- |Message monad with underlying 'IO' monad
type MessageIO = MessageT IO

-- |Convert a non-IO 'MessageM' action into a 'MessageIO' action
toIO :: MessageM a -> MessageIO a
toIO msg = case runMsg msg of
  Left  e         -> throwError e
  Right (x, msgs) -> tell msgs >> return x

-- |Convert a non-IO 'MessageM' action into a 'MessageIO' action
fromIO :: MessageIO a -> IO (MessageM a)
fromIO msgio = do
  res <- runMessageT msgio
  case res of
    Left e          -> return $ throwError e
    Right (x, msgs) -> return $ tell msgs >> return x
