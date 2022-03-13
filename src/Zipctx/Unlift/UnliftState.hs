{-# LANGUAGE UndecidableInstances #-}

module Zipctx.Unlift.UnliftState
  ( UnliftStateT (..)
  , UnliftStateM
  , runUnliftStateT
  , evalUnliftStateT
  , runUnliftStateM
  , evalUnliftStateM
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Zipctx.Structure.MonadStack (MonadStack (..))

newtype UnliftStateT s m a = UnliftStateT { unUnliftStateT :: ReaderT (IORef s) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

type UnliftStateM s = UnliftStateT s IO

instance MonadTrans (UnliftStateT s) where
  lift = UnliftStateT . lift

instance MonadIO m => MonadState s (UnliftStateT s m) where
  get = UnliftStateT (ask >>= liftIO . readIORef)
  put s = UnliftStateT (ask >>= liftIO . flip writeIORef s)
  state f = UnliftStateT (ask >>= liftIO . flip atomicModifyIORef' (\s -> let (a, s') = f s in (s', a)))

instance MonadStack x m => MonadStack x (UnliftStateT s m) where
  nullStackM = lift nullStackM
  pushStackM = lift . pushStackM
  popStackM = lift popStackM
  peekStackM = lift peekStackM
  foldStackM = lift . foldStackM
  revertStackM = lift . revertStackM
  revertMapStackM = lift . revertMapStackM
  reflectStackM = lift reflectStackM

runUnliftStateT :: MonadIO m => UnliftStateT s m a -> s -> m (a, s)
runUnliftStateT m s = do
  ref <- liftIO (newIORef s)
  a <- runReaderT (unUnliftStateT m) ref
  s' <- liftIO (readIORef ref)
  pure (a, s')

evalUnliftStateT :: MonadIO m => UnliftStateT s m a -> s -> m a
evalUnliftStateT m = fmap fst . runUnliftStateT m

runUnliftStateM :: UnliftStateM s a -> s -> IO (a, s)
runUnliftStateM = runUnliftStateT

evalUnliftStateM :: UnliftStateM s a -> s -> IO a
evalUnliftStateM = evalUnliftStateT
