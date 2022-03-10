{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Zipctx where

type Var = String
type Op = String

data Lit =
    LitInt !Int
  | LitBool !Bool
  deriving stock (Eq, Show)

data Exp =
    ExpVar !Var
  | ExpOp !Op
  | ExpLit !Lit
  | ExpLet !Var Exp Exp
  | ExpApp Exp ![Exp]
  deriving stock (Eq, Show)

-- All the ExpF can be derived with recursion-schemes
-- but dependency-free this is the best we can do

data ExpF a =
    ExpVarF !Var
  | ExpOpF !Op
  | ExpLitF !Lit
  | ExpLetF !Var a a
  | ExpAppF a ![a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

projectExp :: Exp -> ExpF Exp
projectExp = \case
  ExpVar v -> ExpVarF v
  ExpOp o -> ExpOpF o
  ExpLit l -> ExpLitF l
  ExpLet v b e -> ExpLetF v b e
  ExpApp l rs -> ExpAppF l rs

data Value =
    ValueVar !Var
  | ValueOp !Op
  | ValueLit !Lit
  deriving stock (Eq, Show)

data Ctx x y a =
    CtxHole a
  | CtxLetBind !Var (Ctx x y a) x
  | CtxLetBody !Var y (Ctx x y a)
  | CtxAppLeft (Ctx x y a) ![x]
  | CtxAppRight y ![y] (Ctx x y a) ![x]
  deriving stock (Eq, Show)

data CtxF x y a b =
    CtxHoleF a
  | CtxLetBindF !Var b x
  | CtxLetBodyF !Var y b
  | CtxAppLeftF b ![x]
  | CtxAppRightF y ![y] b ![x]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

projectCtx :: Ctx x y a -> CtxF x y a (Ctx x y a)
projectCtx = \case
  CtxHole a -> CtxHoleF a
  CtxLetBind s ctx x -> CtxLetBindF s ctx x
  CtxLetBody s y ctx -> CtxLetBodyF s y ctx
  CtxAppLeft ctx xs -> CtxAppLeftF ctx xs
  CtxAppRight y ys ctx xs -> CtxAppRightF y ys ctx xs

-- extract :: Ctx x y a -> a
-- extract = \case
--   CtxHole a -> a
--   CtxLetBind _ a _ -> a
--   CtxLetBody _ _ a -> a
--   CtxAppLeft a _ -> a
--   CtxAppRight _ _ a _ -> a

-- extend :: (Ctx x y a -> b) -> (Ctx x y a -> Ctx x y b)
-- extend f = CtxHole . f

data Kont x y =
    KontHole
  | KontLetBind !Var x
  | KontLetBody !Var y
  | KontAppLeft ![x]
  | KontAppRight y ![y] ![x]
  deriving stock (Eq, Show)

data Redex a = RedexApp a ![a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

advance :: (a -> y) -> Ctx x y a -> Either (Redex y) (Ctx x y a)
advance = undefined

select :: Exp -> Either Value (Ctx Exp Value Exp)
select = \case
  ExpVar v -> Left (ValueVar v)
  ExpOp o -> Left (ValueOp o)
  ExpLit l -> Left (ValueLit l)
  ExpLet v b e -> Right (CtxLetBind v b e)
  ExpApp l rs -> Right (CtxAppLeft l rs)

eval :: Monad m => (Redex Value -> m Value) -> Exp -> m Value
eval f = go . select where
  go = \case
    Left v -> pure v
    Right c -> undefined
