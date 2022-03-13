module Zipctx.Scope where

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

data AnnoExp l c a = AnnoExp !(l c) !a
  deriving stock (Eq, Show)

instance Monoid (l c) => Scoped (AnnoScope l) c (AnnoExp l c a) where
  localScope (AnnoScope lcX) (AnnoExp lcY a) = AnnoExp (lcX <> lcY) a
