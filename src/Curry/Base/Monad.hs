module Curry.Base.Monad
  ( CYIO, CYM, CYT, failMessages, failMessageAt, ok, runCYIO, runCYM, liftCYM
  ) where

import Control.Monad.Identity
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT, mapEitherT)

import Curry.Base.Message  (Message, posMessage)
import Curry.Base.Position
import Curry.Base.Pretty   (text)

type CYT m a = EitherT [Message] m a

type CYIO a = EitherT [Message] IO a

type CYM a = EitherT [Message] Identity a

runCYIO :: CYIO a -> IO (Either [Message] a)
runCYIO = runEitherT

runCYM :: CYM a -> Either [Message] a
runCYM = runIdentity . runEitherT

failMessage :: Monad m => Message -> CYT m a
failMessage msg = failMessages [msg]

failMessages :: Monad m => [Message] -> CYT m a
failMessages = left

failMessageAt :: Monad m => Position -> String -> CYT m a
failMessageAt pos s = failMessage $ posMessage pos $ text s

ok :: Monad m => a -> CYT m a
ok = right

liftCYM :: Monad m => CYM a -> CYT m a
liftCYM = mapEitherT (return . runIdentity)