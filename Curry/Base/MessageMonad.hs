{-# LANGUAGE FlexibleContexts #-}
{-
  The monads MsgMonad and MsgMonadIO provide a common way
  to log warning messages and to stop execution when an
  error occurs. They may be used to integrate different
  compiler passes smoothly.

  (c) 2009, Holger Siegel.

-}

module Curry.Base.MessageMonad where

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Identity

import Curry.Base.Position


type MsgMonadT m = ErrorT WarnMsg (WriterT [WarnMsg] m)

type MsgMonad = MsgMonadT Identity

type MsgMonadIO = MsgMonadT IO

data WarnMsg = WarnMsg { warnPos :: Maybe Position,
                         warnTxt :: String
                       }
instance Error WarnMsg where
    noMsg = WarnMsg Nothing "Failure!"
    strMsg = WarnMsg Nothing

instance Show WarnMsg where
    show = showWarning

showWarning w = "Warning: " ++ pos ++ warnTxt w
    where pos = case warnPos w of
                  Nothing -> ""
                  Just p -> show p ++": "

showError w = "Error: " ++ pos ++ warnTxt w
    where pos = case warnPos w of
                  Nothing -> ""
                  Just p -> show p ++": "

ok :: MsgMonad a -> a
ok = either (error . showError) id . fst . runMsg


failWith :: (MonadError a m, Error a) => String -> m a1
failWith = throwError . strMsg


failWithAt :: (MonadError WarnMsg m) => Position -> String -> m a
failWithAt p = throwError . WarnMsg (Just p)


warnMessage :: (MonadWriter [WarnMsg] m) => String -> m ()
warnMessage s = tell [WarnMsg Nothing s]


warnMessageAt :: (MonadWriter [WarnMsg] m) => Position -> String -> m ()
warnMessageAt p s  = tell [WarnMsg (Just p) s]

runMsg :: MsgMonad a -> (Either WarnMsg a, [WarnMsg])
runMsg = runIdentity . runWriterT . runErrorT

-- returnIO :: MsgMonad a -> MsgMonadIO a
-- returnIO x = return$ (runIdentity . runWriterT . runErrorT) x

runMsgIO :: MsgMonad a -> (a -> IO (MsgMonad b)) -> IO (MsgMonad b)
runMsgIO m f
    = case runMsg m of
        (Left e, msgs) -> return (tell msgs >> throwError e)
        (Right x, msgs) -> do m' <- f x
                              case runMsg m' of
                                (Left _,_) -> return m'
                                (Right x', msgs') -> return (tell (msgs ++ msgs') >> return x')

dropIO :: MsgMonad a -> MsgMonadIO a
dropIO x = case runMsg x of
             (Left e, m) -> tell m >> throwError e
             (Right x, m) -> tell m >> return x