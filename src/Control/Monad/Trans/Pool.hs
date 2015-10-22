{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Pool (WithPoolT, WithPool, runWithPool, withPool) where

import Control.Arrow (Kleisli(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Free.Church (FT, toFT, iterT, liftF)

import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda)
import Data.Pool (Pool, withResource)

newtype WithPoolF r m a = WithPoolF (Coyoneda (Kleisli m r) a)
  deriving Functor

type WithPool r a = WithPoolT r IO a

newtype WithPoolT r m a = WithPoolT (FT (WithPoolF r m) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (WithPoolT r) where
  lift = WithPoolT . lift

runWithPoolF :: MonadBaseControl IO m => Pool r -> WithPoolF r m (m a) -> m a
runWithPoolF pool (WithPoolF (Coyoneda next k)) = withResource pool (runKleisli k) >>= next

runWithPool :: MonadBaseControl IO m => WithPoolT r m a -> Pool r -> m a
runWithPool (WithPoolT m) pool = iterT (runWithPoolF pool) m

withPool :: Monad m => (r -> m a) -> WithPoolT r m a
withPool = WithPoolT . liftF . WithPoolF . liftCoyoneda . Kleisli
