{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- | Reduction semantics based on "Clowns to the Left of me, Jokers to the Right" by Conor McBride
-- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.6134&rep=rep1&type=pdf
-- Susp and Dissectable adapted from https://reasonablypolymorphic.com/blog/clowns-jokers/index.html
module Zipctx where

import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Functor.Foldable (Base, Recursive (..), Corecursive (..))
import Data.Map.Strict (Map)
import Data.Functor (($>))
import Control.Monad.Reader (ReaderT, MonadReader (..), runReaderT, asks)
import Control.Monad.Except (MonadError (..), Except, runExcept)
import Control.Monad.State (StateT, MonadState (..), evalStateT, gets, runStateT)
import Zipctx.Scope (AnnoExp (..), Scoped (..))
import Data.Proxy (Proxy)
import Zipctx.Structure.Stack (Stack)
import Zipctx.Structure.MonadStack (MonadStack (..))
-- import Zipctx.Structure.StackT (StackT, runStackT)
import Zipctx.Structure.StateStack (StateStackT, runStateStackT)

-- | Elem
data Elem f r v c j =
    ElemReturn !v
  | ElemFocus !(f j)
  | ElemReduce !(r c j)
  deriving stock (Eq, Show)

instance (Functor f, Bifunctor r) => Bifunctor (Elem f r v) where
  bimap f g = go where
    go = \case
      ElemReturn v -> ElemReturn v
      ElemFocus fj -> ElemFocus (fmap g fj)
      ElemReduce rcj -> ElemReduce (bimap f g rcj)

-- | Reduction
data Reduction f v j =
    ReductionReturn !v
  | ReductionFocus !(f j)
  deriving stock (Eq, Show, Functor)

-- | Focus
data Focus d v c j =
    FocusReturn !v
  | FocusDissect !j !(d c j)
  deriving stock (Eq, Show)

instance Bifunctor d => Bifunctor (Focus d v) where
  bimap f g = go where
    go = \case
      FocusReturn v -> FocusReturn v
      FocusDissect j d -> FocusDissect (g j) (bimap f g d)

data Step r d c j =
    StepReturn !c
  | StepReduce !(r c j)
  | StepDissect !j !(d c j)
  deriving stock (Eq, Show)

instance (Bifunctor r, Bifunctor d) => Bifunctor (Step r d) where
  bimap f g = \case
    StepReturn c -> StepReturn (f c)
    StepReduce r -> StepReduce (bimap f g r)
    StepDissect j d -> StepDissect (g j) (bimap f g d)

class (Bifunctor r, Functor (RedExp r), Monad (RedEffect r), Base (RedUneval r) ~ RedExp r) => Reducible r where
  -- | Expression functor
  type family RedExp r :: Type -> Type

  -- | Some monad capturing the effect of evaluation (e.g. reader, error, state, and their combinations)
  type family RedEffect r :: Type -> Type

  -- | Intermediate state of evaluation
  type family RedEval r :: Type

  -- | Unevaluated expressions
  type family RedUneval r :: Type

  -- | Evaluates the redex
  redexReduce :: r (RedEval r) (RedUneval r) -> RedEffect r (Reduction (RedExp r) (RedEval r) (RedUneval r))

class (Bifunctor d, Functor (DisExp d)) => Dissectable d where
  type family DisExp d :: Type -> Type

  type family DisEval d :: Type

  type family DisLocal d :: Type -> Type

  type family DisRed d :: Type -> Type -> Type

  -- | Focus on the next element
  disFocus :: DisExp d j -> Focus d (DisEval d) c j

  -- | Advance one step in the dissection
  disStep :: Scoped (DisLocal d) c j => c -> d c j -> Step (DisRed d) d c j

class (
    Functor f, Monad (LangEffect f),
    Reducible (LangRed f), RedExp (LangRed f) ~ f, RedEval (LangRed f) ~ LangEval f,
    RedEval (LangRed f) ~ LangEval f, RedUneval (LangRed f) ~ LangUneval f, RedEffect (LangRed f) ~ LangEffect f,
    Dissectable (LangDis f), DisExp (LangDis f) ~ f, DisEval (LangDis f) ~ LangEval f,
    DisLocal (LangDis f) ~ LangLocal f, DisRed (LangDis f) ~ LangRed f,
    Scoped (LangLocal f) (LangEval f) (LangUneval f)
  ) => Language f where
  type LangRed f :: Type -> Type -> Type
  type LangDis f :: Type -> Type -> Type
  type LangEval f :: Type
  type LangUneval f :: Type
  type LangEffect f :: Type -> Type
  type LangLocal f :: Type -> Type

newtype MachineElem f = MachineElem (Elem f (LangRed f) (LangEval f) (LangEval f) (LangUneval f))
newtype MachineDis f = MachineDis (LangDis f (LangEval f) (LangUneval f))

newtype MachineM f a = MachineM { unMachineM :: StateStackT (MachineElem f) (MachineDis f) (LangEffect f) a }

deriving newtype instance Functor (LangEffect f) => Functor (MachineM f)
deriving newtype instance Monad (LangEffect f) => Applicative (MachineM f)
deriving newtype instance Monad (LangEffect f) => Monad (MachineM f)
deriving newtype instance Monad (LangEffect f) => MonadStack (MachineDis f) (MachineM f)
deriving newtype instance Monad (LangEffect f) => MonadState (MachineElem f) (MachineM f)

runMachineM :: MachineM f a -> MachineElem f -> Stack (MachineDis f) -> m (a, MachineElem f, Stack (MachineDis f))
runMachineM = error "TODO"

-- eval :: (Recursive (Uneval f), Dissectable f) => Uneval f -> Effect f (ElemF f, DisStack f)
-- eval = error "TODO"

evalM :: Language f => MachineM f Bool
evalM = do
  MachineElem elem <- get
  case elem of
    ElemReturn res -> do
      md <- popStackM
      case md of
        Nothing -> pure True
        Just (MachineDis d) ->
          case disStep res d of
            StepReturn c -> put (MachineElem (ElemReturn c)) $> False
            StepReduce red -> undefined
            StepDissect j ds' -> undefined
    ElemFocus jf -> error "TODO"
    ElemReduce red -> error "TODO"

-- Example follows:

-- | We'll just use strings for variables.
-- These might include operation names like +, *, etc.
type Var = String

-- | Literals for our language
-- (Ignore the exclamation points when you see them, they are strictness annotations.)
data Lit =
    LitInt !Int
  | LitBool !Bool
  deriving stock (Eq, Show)

-- | Values for our language
data Value =
    ValueVar !Var
  | ValueLit !Lit
  deriving stock (Eq, Show)

-- | Our expression language includes vars, literals, ifs, lets, and applications
data Exp =
    ExpVar !Var
  | ExpLit !Lit
  | ExpIf Exp Exp Exp
  | ExpLet !Var Exp Exp
  | ExpApp Exp !(Seq Exp)
  deriving stock (Eq, Show)

-- All the ExpF/Recursive/Corecursive stuff can be derived with TemplateHaskell
-- but for this example it's clearer to just show everything.

-- | The "base functor" of our expression language.
-- Basically, we "punch a hole" in each constructor where they refer to recursive parts.
-- Note that we could take this as given and define 'Exp' as a fixpoint over this
-- (Something like `newtype Exp = Exp (ExpF Exp)`)
data ExpF a =
    ExpVarF !Var
  | ExpLitF !Lit
  | ExpIfF a a a
  | ExpLetF !Var a a
  | ExpAppF a !(Seq a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- | Boilerplate for `recursion-schemes`
type instance Base Exp = ExpF

-- | Boilerplate for `recursion-schemes`
instance Recursive Exp where
  project = \case
    ExpVar v -> ExpVarF v
    ExpLit l -> ExpLitF l
    ExpIf g t e -> ExpIfF g t e
    ExpLet v b e -> ExpLetF v b e
    ExpApp l rs -> ExpAppF l rs

-- | Boilerplate for `recursion-schemes`
instance Corecursive Exp where
  embed = \case
    ExpVarF v -> ExpVar v
    ExpLitF l -> ExpLit l
    ExpIfF g t e -> ExpIf g t e
    ExpLetF v b e -> ExpLet v b e
    ExpAppF l rs -> ExpApp l rs

-- | Redexes corresponding to our expressions.
-- The 'c' parts are already evaluated, and the 'j' parts are not.
data ExpRed c j =
    ExpRedIf c j j
  | ExpRedApp c !(Seq c)
  deriving stock (Eq, Show)

instance Bifunctor ExpRed where
  bimap f g = \case
    ExpRedIf c jt je -> ExpRedIf (f c) (g jt) (g je)
    ExpRedApp h tl -> ExpRedApp (f h) (fmap f tl)

-- | A 'Dissection' of our expressions.
-- The 'c' and 'j' parts denote evaluated/unevaluated parts as before.
-- We can "step" through this
data ExpDis c j =
    ExpDisIf j j
  | ExpDisLetBind !Var j
  | ExpDisLetBody
  | ExpDisAppHead !(Seq j)
  | ExpDisAppTail c !(Seq c) !(Seq j)
  deriving stock (Eq, Show)

instance Bifunctor ExpDis where
  bimap f g = \case
    ExpDisIf jt je -> ExpDisIf (g jt) (g je)
    ExpDisLetBind v j -> ExpDisLetBind v (g j)
    ExpDisLetBody -> ExpDisLetBody
    ExpDisAppHead js -> ExpDisAppHead (fmap g js)
    ExpDisAppTail c cs js -> ExpDisAppTail (f c) (fmap f cs) (fmap g js)

data ExpLocal c = ExpLocal !Var !c
  deriving stock (Eq, Show)

type Error = String
type VarEnv = Map Var Value

newtype ExpM a = ExpM { unExpM :: ReaderT VarEnv (Except Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader VarEnv, MonadError Error)

runExpM :: ExpM a -> VarEnv -> Either Error a
runExpM m ve = runExcept (runReaderT (unExpM m) ve)

instance MonadFail ExpM where
  fail = throwError

instance Reducible ExpRed where
  type RedExp ExpRed = ExpF
  type RedEffect ExpRed = ExpM
  type RedEval ExpRed = Value
  type RedUneval ExpRed = AnnoExp ExpLocal Value Exp

  redexReduce = \case
    ExpRedIf c jt je ->
      case c of
        ValueLit (LitBool b) ->
          pure (ReductionFocus (traverse project (if b then jt else je)))
        _ -> fail "non-boolean guard"
    ExpRedApp h tl -> error "TODO"

instance Dissectable ExpDis where
  type DisExp ExpDis = ExpF
  type DisEval ExpDis = Value
  type DisLocal ExpDis = ExpLocal
  type DisRed ExpDis = ExpRed

  disFocus = \case
    ExpVarF v -> FocusReturn (ValueVar v)
    ExpLitF l -> FocusReturn (ValueLit l)
    ExpIfF g t e -> FocusDissect g (ExpDisIf t e)
    ExpLetF v e b -> FocusDissect e (ExpDisLetBind v b)
    ExpAppF h tl -> FocusDissect h (ExpDisAppHead tl)

  disStep c = \case
    ExpDisLetBind v jb -> StepDissect (localScope (ExpLocal v c) jb) ExpDisLetBody
    ExpDisLetBody -> StepReturn c
    ExpDisIf jt je -> StepReduce (ExpRedIf c jt je)
    ExpDisAppHead jtl ->
      case jtl of
        Empty -> StepReduce (ExpRedApp c Empty)
        j :<| jtl' -> StepDissect j (ExpDisAppTail c Empty jtl')
    ExpDisAppTail chd ctl jtl ->
      case jtl of
        Empty -> StepReduce (ExpRedApp chd (ctl :|> c))
        j :<| jtl' -> StepDissect j (ExpDisAppTail c (ctl :|> c) jtl')

instance Language ExpF where
  type LangEffect ExpF = ExpM
  type LangEval ExpF = Value
  type LangUneval ExpF = AnnoExp ExpLocal Value Exp
  type LangEval ExpF = Value
  type LangLocal ExpF = ExpLocal
  type LangRed ExpF = ExpRed
  type LangDis ExpF = ExpDis
