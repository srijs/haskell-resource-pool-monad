{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Pool (WithResourceT, WithResource, withResource, tryWithResource, runPooled, runDedicated) where

import Control.Arrow (Kleisli(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Free.Church (FT, toFT, iterT, liftF)

import Data.Functor.Coyoneda (Coyoneda(..), liftCoyoneda)
import Data.Functor.Kan.Lan (Lan(..), glan)
import Data.Pool (Pool)
import qualified Data.Pool as Pool (tryWithResource, withResource)

data WithResourceF r m a = WithResource (Coyoneda (Kleisli m r) a)
                         | TryWithResource (Lan Maybe (Kleisli m r) a)
  deriving Functor

type WithResource r a = WithResourceT r IO a

newtype WithResourceT r m a = WithResourceT (FT (WithResourceF r m) m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (WithResourceT r) where
  lift = WithResourceT . lift

runPooledF :: MonadBaseControl IO m => Pool r -> WithResourceF r m (m a) -> m a
runPooledF pool (WithResource (Coyoneda next k)) = Pool.withResource pool (runKleisli k) >>= next
runPooledF pool (TryWithResource (Lan next k)) = Pool.tryWithResource pool (runKleisli k) >>= next

runPooled :: MonadBaseControl IO m => WithResourceT r m a -> Pool r -> m a
runPooled (WithResourceT m) pool = iterT (runPooledF pool) m

runDedicatedF :: Monad m => r -> WithResourceF r m (m a) -> m a
runDedicatedF r (WithResource (Coyoneda next k)) = runKleisli k r >>= next
runDedicatedF r (TryWithResource (Lan next k)) = runKleisli k r >>= next . Just

runDedicated :: Monad m => WithResourceT r m a -> r -> m a
runDedicated (WithResourceT m) r = iterT (runDedicatedF r) m

withResource :: Monad m => (r -> m a) -> WithResourceT r m a
withResource = WithResourceT . liftF . WithResource . liftCoyoneda . Kleisli

tryWithResource :: Monad m => (r -> m a) -> WithResourceT r m (Maybe a)
tryWithResource = WithResourceT . liftF . TryWithResource . glan . Kleisli
