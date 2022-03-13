{-# LANGUAGE UndecidableInstances #-}

module Zipctx.Structure.MonadStack
  ( MonadStack (..)
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans (lift)
import Data.Functor.Foldable (embed)
import Data.Maybe (isNothing)
import Zipctx.Structure.Stack (Stack, StackF)

-- | Monad encapsulating mutable-stack-like effects.
-- It gets interesting when you start saving and restoring state on the stack.
-- The MTL boilerplate lets you put stack-like monads at the base of the stack and
-- layer Reader/Except/State on top.
class Monad m => MonadStack x m | m -> x where
  pushStackM :: x -> m ()
  popStackM :: m (Maybe x)
  peekStackM :: m (Maybe x)
  foldStackM :: (StackF x a -> a) -> m a

  nullStackM :: m Bool
  nullStackM = fmap isNothing peekStackM

  revertStackM :: (x -> Bool) -> m (Maybe x)
  revertStackM f = go where
    go = do
      mx <- popStackM
      case mx of
        Just a | not (f a) -> go
        _ -> pure mx

  revertMapStackM :: (x -> Maybe y) -> m (Maybe y)
  revertMapStackM f = go where
    go = do
      mx <- popStackM
      case mx of
        Just a ->
          case f a of
            Nothing -> go
            my@(Just _) -> pure my
        Nothing -> pure Nothing

  reflectStackM :: m (Stack x)
  reflectStackM = foldStackM embed

instance MonadStack x m => MonadStack x (ReaderT r m) where
  nullStackM = lift nullStackM
  pushStackM = lift . pushStackM
  popStackM = lift popStackM
  peekStackM = lift peekStackM
  foldStackM = lift . foldStackM
  revertStackM = lift . revertStackM
  revertMapStackM = lift . revertMapStackM
  reflectStackM = lift reflectStackM

instance MonadStack x m => MonadStack x (ExceptT e m) where
  nullStackM = lift nullStackM
  pushStackM = lift . pushStackM
  popStackM = lift popStackM
  peekStackM = lift peekStackM
  foldStackM = lift . foldStackM
  revertStackM = lift . revertStackM
  revertMapStackM = lift . revertMapStackM
  reflectStackM = lift reflectStackM

instance MonadStack x m => MonadStack x (StateT s m) where
  nullStackM = lift nullStackM
  pushStackM = lift . pushStackM
  popStackM = lift popStackM
  peekStackM = lift peekStackM
  foldStackM = lift . foldStackM
  revertStackM = lift . revertStackM
  revertMapStackM = lift . revertMapStackM
  reflectStackM = lift reflectStackM
