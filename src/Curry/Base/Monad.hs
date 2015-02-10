{- |
    Module      :  $Header$
    Description :  Monads for message handling
    Copyright   :  2014 - 2015 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental

    The monads defined in this module provide a common way to stop execution
    when some errors occur. They are used to integrate different compiler passes
    smoothly.
-}

module Curry.Base.Monad
  ( CYIO, CYM, CYT, failMessages, failMessageAt, ok, runCYIO, runCYM, liftCYM
  ) where

import Control.Monad.Identity
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT, mapEitherT)

import Curry.Base.Message  (Message, posMessage)
import Curry.Base.Position
import Curry.Base.Pretty   (text)

-- |Curry compiler monad transformer
type CYT m a = EitherT [Message] m a

-- |Curry compiler monad based on the `IO` monad
type CYIO a = EitherT [Message] IO a

-- |Pure Curry compiler monad
type CYM a = EitherT [Message] Identity a

-- |Run an `IO`-based Curry compiler action in the `IO` monad,
-- yielding either a list of errors or a result in case of success.
runCYIO :: CYIO a -> IO (Either [Message] a)
runCYIO = runEitherT

-- |Run an pure Curry compiler action,
-- yielding either a list of errors or a result in case of success.
runCYM :: CYM a -> Either [Message] a
runCYM = runIdentity . runEitherT

-- |Failing action with a message describing the cause of failure.
failMessage :: Monad m => Message -> CYT m a
failMessage msg = failMessages [msg]

-- |Failing action with a list of messages describing the cause(s) of failure.
failMessages :: Monad m => [Message] -> CYT m a
failMessages = left

-- |Failing action with a source code position and a `String` indicating
-- the cause of failure.
failMessageAt :: Monad m => Position -> String -> CYT m a
failMessageAt pos s = failMessage $ posMessage pos $ text s

-- |Lift a value into the `CYT m` monad, same as `return`.
ok :: Monad m => a -> CYT m a
ok = right

-- |Lift a pure action into an action based on another monad.
liftCYM :: Monad m => CYM a -> CYT m a
liftCYM = mapEitherT (return . runIdentity)
