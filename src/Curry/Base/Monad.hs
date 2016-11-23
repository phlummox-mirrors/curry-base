{- |
    Module      :  $Header$
    Description :  Monads for message handling
    Copyright   :  2014 - 2016 Björn Peemöller
    License     :  BSD-3-clause

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental

    The monads defined in this module provide a common way to stop execution
    when some errors occur. They are used to integrate different compiler passes
    smoothly.
-}

module Curry.Base.Monad
  ( CYIO, CYM, CYT, failMessages, failMessageAt, warnMessages, warnMessageAt
  , ok, runCYIO, runCYM, runCYIOIgnWarn, runCYMIgnWarn, liftCYM, silent
  ) where

import Control.Monad.Identity
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT, mapEitherT)
import Control.Monad.Writer

import Curry.Base.Message  (Message, posMessage)
import Curry.Base.Position
import Curry.Base.Pretty   (text)

-- |Curry compiler monad transformer
type CYT m a = WriterT [Message] (EitherT [Message] m) a

-- |Curry compiler monad based on the `IO` monad
type CYIO a = CYT IO a

-- |Pure Curry compiler monad
type CYM a = CYT Identity a

-- |Run an `IO`-based Curry compiler action in the `IO` monad,
-- yielding either a list of errors or a result in case of success
-- consisting of the actual result and a (possibly empty) list of warnings
runCYIO :: CYIO a -> IO (Either [Message] (a, [Message]))
runCYIO = runEitherT . runWriterT

-- |Run an pure Curry compiler action,
-- yielding either a list of errors or a result in case of success
-- consisting of the actual result and a (possibly empty) list of warnings
runCYM :: CYM a -> Either [Message] (a, [Message])
runCYM = runIdentity . runEitherT . runWriterT

-- |Run an `IO`-based Curry compiler action in the `IO` monad,
-- yielding either a list of errors or a result in case of success.
runCYIOIgnWarn :: CYIO a -> IO (Either [Message] a)
runCYIOIgnWarn = runEitherT . (liftM fst) . runWriterT

-- |Run an pure Curry compiler action,
-- yielding either a list of errors or a result in case of success.
runCYMIgnWarn :: CYM a -> Either [Message] a
runCYMIgnWarn = runIdentity . runEitherT . (liftM fst) . runWriterT

-- |Failing action with a message describing the cause of failure.
failMessage :: Monad m => Message -> CYT m a
failMessage msg = failMessages [msg]

-- |Failing action with a list of messages describing the cause(s) of failure.
failMessages :: Monad m => [Message] -> CYT m a
failMessages = lift . left

-- |Failing action with a source code position and a `String` indicating
-- the cause of failure.
failMessageAt :: Monad m => Position -> String -> CYT m a
failMessageAt pos s = failMessage $ posMessage pos $ text s

-- |Warning with a message describing the cause of the warning.
warnMessage :: Monad m => Message -> CYT m ()
warnMessage msg = warnMessages [msg]

-- |Warning with a list of messages describing the cause(s) of the warnings.
warnMessages :: Monad m => [Message] -> CYT m ()
warnMessages msgs = tell msgs

-- |Execute a monadic action, but ignore any warnings it issues
silent :: Monad m => CYT m a -> CYT m a
silent act = censor (const []) act

-- |Warning with a source code position and a `String` indicating
-- the cause of the warning.
warnMessageAt :: Monad m => Position -> String -> CYT m ()
warnMessageAt pos s = warnMessage $ posMessage pos $ text s

-- |Lift a value into the `CYT m` monad, same as `return`.
ok :: Monad m => a -> CYT m a
ok = lift . right

-- |Lift a pure action into an action based on another monad.
liftCYM :: Monad m => CYM a -> CYT m a
liftCYM = mapWriterT (mapEitherT (return . runIdentity))
