{-# LANGUAGE UndecidableInstances #-}

module Zipctx.Unlift.UnliftStack
  ( UnliftStackT (..)
  , UnliftStackM
  , runUnliftStackT
  , evalUnliftStackT
  , runUnliftStackM
  , evalUnliftStackM
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.State.Strict (MonadState (..), gets, modify')
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Zipctx.Structure.MonadStack (MonadStack (..))
import Zipctx.Structure.Stack (StackF (..))
import Zipctx.Structure.StateStack (StateStack (..), foldStateStack, nullStateStack, peekStateStack, popStateStack,
                                    reflectStateStack)
import Zipctx.Unlift.UnliftState (UnliftStateT, runUnliftStateT)

newtype UnliftStackT s x m a = UnliftStackT { unUnliftStackT :: UnliftStateT (StateStack s x) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

type UnliftStackM s x = UnliftStackT s x IO

instance MonadTrans (UnliftStackT s x) where
  lift = UnliftStackT . lift

instance MonadIO m => MonadState s (UnliftStackT s x m) where
  get = UnliftStackT (fmap ssState get)
  put s = UnliftStackT (modify' (\st -> st { ssState = s }))
  state f = UnliftStackT (state (\st -> let s = ssState st in let (a, s') = f s in (a, st { ssState = s' })))

instance MonadIO m => MonadStack x (UnliftStackT s x m) where
  nullStackM = UnliftStackT (gets nullStateStack)
  pushStackM x = UnliftStackT (modify' (\ss@(StateStack s _) -> StateStack s (StackCons x ss)))
  popStackM = UnliftStackT (state (\ss -> maybe (Nothing, ss) (first Just) (popStateStack ss)))
  peekStackM = UnliftStackT (gets peekStateStack)
  foldStackM f = UnliftStackT (gets (foldStateStack f))
  reflectStackM = UnliftStackT (gets reflectStateStack)

runUnliftStackT :: MonadIO m => UnliftStackT s x m a -> StateStack s x -> m (a, StateStack s x)
runUnliftStackT = runUnliftStateT . unUnliftStackT

evalUnliftStackT :: MonadIO m => UnliftStackT s x m a -> StateStack s x -> m a
evalUnliftStackT m = fmap fst . runUnliftStackT m

runUnliftStackM :: UnliftStackM s x a -> StateStack s x -> IO (a, StateStack s x)
runUnliftStackM = runUnliftStackT

evalUnliftStackM :: UnliftStackM s x a -> StateStack s x -> IO a
evalUnliftStackM = evalUnliftStackT
