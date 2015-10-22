module Control.Monad.Pool (WithResource, withResource, tryWithResource, runPooled, runDedicated) where

import qualified Control.Monad.Trans.Pool as T

import Data.Pool (Pool)

newtype WithResource r a = WithResource (T.WithResource r a)

runPooled :: WithResource r a -> Pool r -> IO a
runPooled (WithResource m) = T.runPooled m

runDedicated :: WithResource r a -> r -> IO a
runDedicated (WithResource m) = T.runDedicated m

withResource :: (r -> IO a) -> WithResource r a
withResource = WithResource . T.withResource

tryWithResource :: (r -> IO a) -> WithResource r (Maybe a)
tryWithResource = WithResource . T.tryWithResource
