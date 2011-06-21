{- |The monads MsgMonad and MsgMonadIO provide a common way to log warning
    messages and to stop execution when an error occurs. They may be used to
    integrate different compiler passes smoothly.

    (c) 2009, Holger Siegel.
-}

{-# LANGUAGE FlexibleContexts #-}
module Curry.Base.MessageMonad where

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Identity

import Curry.Base.Position

-- |Data type for messages
data Message = Message
  { msgPos :: Maybe Position -- ^ optional source code position
  , msgTxt :: String         -- ^ the message itself
  }

instance Show Message where
  showsPrec _ (Message Nothing txt) = showString txt
  showsPrec _ (Message (Just p)txt) = shows p . showString ": "
                                    . showString txt

-- |Show a 'Message' as a warning
showWarning :: Message -> String
showWarning w = "Warning: " ++ show w

-- |Show a 'Message' as an error
showError :: Message -> String
showError w = "Error: " ++ show w

{- |Message monad transformer enabling the reporting of 'Message's as
    warnings and additionally a 'Message' as an error message.
-}
type MsgMonadT m = ErrorT Message (WriterT [Message] m)

-- |Simple message monad
type MsgMonad = MsgMonadT Identity

-- |Message monad with underlying 'IO' monad
type MsgMonadIO = MsgMonadT IO

instance Error Message where
  noMsg  = Message Nothing "Failure!"
  strMsg = Message Nothing

-- |Evaluate the value of a 'MsgMonad a'
runMsg :: MsgMonad a -> (Either Message a, [Message])
runMsg = runIdentity . runWriterT . runErrorT

{- |Directly evaluate to the success value of a 'MsgMonad a'. Errors are
    converted in a call to the 'error' function.
-}
ok :: MsgMonad a -> a
ok = either (error . showError) id . fst . runMsg

-- |Sequence 'MsgMonad' action inside the 'IO' monad.
runMsgIO :: MsgMonad a -> (a -> IO (MsgMonad b)) -> IO (MsgMonad b)
runMsgIO m f = case runMsg m of
  (Left  e, msgs) -> return (tell msgs >> throwError e)
  (Right x, msgs) -> do
    m' <- f x
    case runMsg m' of
      (Left _  , _    ) -> return m'
      (Right x', msgs') -> return (tell (msgs ++ msgs') >> return x')

-- |Convert a 'MsgMonad' to a 'MsgMonadIO'
dropIO :: MsgMonad a -> MsgMonadIO a
dropIO m = case runMsg m of
  (Left  e, msgs) -> tell msgs >> throwError e
  (Right x, msgs) -> tell msgs >> return x

-- |Abort the computation with an error message
failWith :: (MonadError a m, Error a) => String -> m b
failWith = throwError . strMsg

-- |Abort the computation with an error message at a certain position
failWithAt :: (MonadError Message m) => Position -> String -> m a
failWithAt p = throwError . Message (Just p)

-- |Report a warning message
warnMessage :: (MonadWriter [Message] m) => String -> m ()
warnMessage s = tell [Message Nothing s]

-- |Report a warning message for a given position
warnMessageAt :: (MonadWriter [Message] m) => Position -> String -> m ()
warnMessageAt p s  = tell [Message (Just p) s]
