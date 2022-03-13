module Zipctx.Scope where

import Data.Functor.Foldable (Base, Recursive (..))

-- | Captures the effect of applying local scope changes
-- Laws: `localScope mempty = id`, `localScope (a <> b) j = localScope a (localScope b j)`
class Monoid (l c) => Scoped l c j where
  localScope :: l c -> j -> j

data NoScope c = NoScope
  deriving stock (Eq, Show)

instance Semigroup (NoScope c) where
  _ <> _ = NoScope

instance Monoid (NoScope c) where
  mempty = NoScope
  mappend = (<>)

instance Scoped NoScope c j where
  localScope _ j = j

-- | Uses the semigroup and monoid instances of the underlying type.
-- Note that the semigroup should be left-biased.
newtype AnnoScope l c = AnnoScope (l c)
  deriving stock (Show)
  deriving newtype (Eq, Semigroup, Monoid)

data AnnoExp l c j = AnnoExp !(l c) !j
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Monoid (l c) => Scoped (AnnoScope l) c (AnnoExp l c j) where
  localScope (AnnoScope lcX) (AnnoExp lcY j) = AnnoExp (lcX <> lcY) j

projectAnnoExp :: Recursive j => AnnoExp l c j -> Base j (AnnoExp l c j)
projectAnnoExp (AnnoExp lc j) = fmap (AnnoExp lc) (project j)
