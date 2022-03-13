module Zipctx.Structure.StackT
  ( StackT (..)
  , runStackT
  , evalStackT
  , StackM
  , runStackM
  , evalStackM
  ) where

import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), StateT (..), gets, modify', state)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Functor.Foldable (cata)
import Zipctx.Structure.MonadStack (MonadStack (..))
import Zipctx.Structure.Stack (Stack, nullStack, peekStack, popStack, pushStack, revertMapStack, revertStack)

newtype StackT x m a = StackT { unStackT :: StateT (Stack x) m a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (StackT x) where
  lift = StackT . lift

instance Monad m => MonadStack x (StackT x m) where
  nullStackM = StackT (gets nullStack)
  pushStackM x = StackT (modify' (pushStack x))
  popStackM = StackT (state (\s -> maybe (Nothing, s) (first Just) (popStack s)))
  peekStackM = StackT (gets peekStack)
  foldStackM f = StackT (gets (cata f))
  revertStackM f = StackT (state (\ss -> maybe (Nothing, ss) (first Just) (revertStack f ss)))
  revertMapStackM f = StackT (state (\ss -> maybe (Nothing, ss) (first Just) (revertMapStack f ss)))
  reflectStackM = StackT get

runStackT :: StackT x m a -> Stack x -> m (a, Stack x)
runStackT = runStateT . unStackT

evalStackT :: Functor m => StackT x m a -> Stack x -> m a
evalStackT m = fmap fst . runStackT m

type StackM x = StackT x Identity

runStackM :: StackM x a -> Stack x -> (a, Stack x)
runStackM m = runIdentity . runStackT m

evalStackM :: StackM x a -> Stack x -> a
evalStackM m = fst . runStackM m
