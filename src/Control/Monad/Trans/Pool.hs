{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Pool (WithResourceT, WithResource, withResource, runPooled, runDedicated) where

import Control.Arrow (Kleisli(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Free.Church (FT, toFT, iterT, liftF)

import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda)
import Data.Pool (Pool)
import qualified Data.Pool as Pool (withResource)

newtype WithResourceF r m a = WithResourceF (Coyoneda (Kleisli m r) a)
  deriving Functor

type WithResource r a = WithResourceT r IO a

newtype WithResourceT r m a = WithResourceT (FT (WithResourceF r m) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (WithResourceT r) where
  lift = WithResourceT . lift

runPooledF :: MonadBaseControl IO m => Pool r -> WithResourceF r m (m a) -> m a
runPooledF pool (WithResourceF (Coyoneda next k)) = Pool.withResource pool (runKleisli k) >>= next

runPooled :: MonadBaseControl IO m => WithResourceT r m a -> Pool r -> m a
runPooled (WithResourceT m) pool = iterT (runPooledF pool) m

runDedicatedF :: Monad m => r -> WithResourceF r m (m a) -> m a
runDedicatedF r (WithResourceF (Coyoneda next k)) = runKleisli k r >>= next

runDedicated :: Monad m => WithResourceT r m a -> r -> m a
runDedicated (WithResourceT m) r = iterT (runDedicatedF r) m

withResource :: Monad m => (r -> m a) -> WithResourceT r m a
withResource = WithResourceT . liftF . WithResourceF . liftCoyoneda . Kleisli
