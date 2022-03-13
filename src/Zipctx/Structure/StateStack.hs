module Zipctx.Structure.StateStack
  ( StateStack (..)
  , emptyStateStack
  , nullStateStack
  , pushStateStack
  , popStateStack
  , peekStateStack
  , revertStateStack
  , revertMapStateStack
  , foldStateStack
  , reflectStateStack
  , StateStackT (..)
  , StateStackM
  , runStateStackT
  , evalStateStackT
  , runStateStackM
  , evalStateStackM
  ) where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Functor.Foldable (embed)
import Zipctx.Structure.MonadStack (MonadStack (..))
import Zipctx.Structure.Stack (Stack, StackF (..))

-- | A recursive pair of (state, stack) such that pushing an element onto the stack
-- saves the current state, and popping an element from the stack restores the saved state.
data StateStack s x = StateStack
  { ssState :: !s
  , ssStack :: StackF x (StateStack s x)
  } deriving stock (Eq, Show)

instance Functor (StateStack s) where
  fmap f = go where
    go (StateStack s st) =
      case st of
        StackNil -> StateStack s StackNil
        StackCons a ss -> StateStack s (StackCons (f a) (go ss))

instance Foldable (StateStack s) where
  foldr f z = go where
    go (StateStack _ st) =
      case st of
        StackNil -> z
        StackCons a st' -> f a (go st')

instance Traversable (StateStack s) where
  traverse f = go1 where
    go1 (StateStack s st) = fmap (StateStack s) (go2 st)
    go2 = \case
      StackNil -> pure StackNil
      StackCons a st' -> liftA2 StackCons (f a) (go1 st')

emptyStateStack :: s -> StateStack s a
emptyStateStack s = StateStack s StackNil

nullStateStack :: StateStack s a -> Bool
nullStateStack (StateStack _ f) =
  case f of
    StackNil -> True
    _ -> False

pushStateStack :: a -> StateStack s a -> StateStack s a
pushStateStack a ss@(StateStack s _) = StateStack s (StackCons a ss)

popStateStack :: StateStack s a -> Maybe (a, StateStack s a)
popStateStack (StateStack _ st) =
  case st of
    StackNil -> Nothing
    StackCons a st' -> Just (a, st')

peekStateStack :: StateStack s a -> Maybe a
peekStateStack (StateStack _ st) =
  case st of
    StackNil -> Nothing
    StackCons a _ -> Just a

revertStateStack :: (a -> Bool) -> StateStack s a -> Maybe (a, StateStack s a)
revertStateStack f = go where
  go (StateStack _ st) =
    case st of
      StackNil -> Nothing
      StackCons a st' -> if f a then Just (a, st') else go st'

revertMapStateStack :: (a -> Maybe b) -> StateStack s a -> Maybe (b, StateStack s a)
revertMapStateStack f = go where
  go (StateStack _ st) =
    case st of
      StackNil -> Nothing
      StackCons a st' ->
        case f a of
          Nothing -> go st'
          Just b -> Just (b, st')

foldStateStack :: (StackF a b -> b) -> StateStack s a -> b
foldStateStack f = go where
  go (StateStack _ st) =
    case st of
      StackNil -> f StackNil
      StackCons a st' -> f (StackCons a (go st'))

reflectStateStack :: StateStack s a -> Stack a
reflectStateStack = foldStateStack embed

-- | Simple state monad maintaining a 'StateStack'.
newtype StateStackT s x m a = StateStackT { unStateStackT :: StateT (StateStack s x) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

type StateStackM s x = StateStackT s x Identity

instance MonadTrans (StateStackT s x) where
  lift = StateStackT . lift

instance Monad m => MonadState s (StateStackT s x m) where
  get = StateStackT (gets ssState)
  put s = StateStackT (modify' (\(StateStack _ ss) -> StateStack s ss))
  state f = StateStackT (state (\(StateStack s ss) -> let (a, s') = f s in (a, StateStack s' ss)))

instance Monad m => MonadStack x (StateStackT s x m) where
  nullStackM = StateStackT (gets nullStateStack)
  pushStackM x = StateStackT (modify' (\ss@(StateStack s _) -> StateStack s (StackCons x ss)))
  popStackM = StateStackT (state (\ss -> maybe (Nothing, ss) (first Just) (popStateStack ss)))
  peekStackM = StateStackT (gets peekStateStack)
  foldStackM f = StateStackT (gets (foldStateStack f))
  revertStackM f = StateStackT (state (\ss -> maybe (Nothing, ss) (first Just) (revertStateStack f ss)))
  revertMapStackM f = StateStackT (state (\ss -> maybe (Nothing, ss) (first Just) (revertMapStateStack f ss)))
  reflectStackM = StateStackT (gets reflectStateStack)

runStateStackT :: StateStackT s x m a -> StateStack s x -> m (a, StateStack s x)
runStateStackT = runStateT . unStateStackT

evalStateStackT :: Functor m => StateStackT s x m a -> StateStack s x -> m a
evalStateStackT m = fmap fst . runStateStackT m

runStateStackM :: StateStackM s x a -> StateStack s x -> (a, StateStack s x)
runStateStackM m = runIdentity . runStateStackT m

evalStateStackM :: StateStackM s x a -> StateStack s x -> a
evalStateStackM m = fst . runStateStackM m
