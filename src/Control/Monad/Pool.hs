module Control.Monad.Pool (WithPool, runWithPool, withPool) where

import qualified Control.Monad.Trans.Pool as T

import Data.Pool

newtype WithPool r a = WithPool (T.WithPoolT r IO a)

runWithPool :: WithPool r a -> Pool r -> IO a
runWithPool (WithPool m) = T.runWithPool m

withPool :: (r -> IO a) -> WithPool r a
withPool use = WithPool $ T.withPool use
