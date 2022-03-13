{-# LANGUAGE UndecidableInstances #-}

-- | Reduction semantics based on "Clowns to the Left of me, Jokers to the Right" by Conor McBride
-- https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.6134&rep=rep1&type=pdf
-- Susp and Dissectable adapted from https://reasonablypolymorphic.com/blog/clowns-jokers/index.html
module Zipctx where

import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Sequence (Seq (..))
import Data.Functor.Foldable (Base, Recursive (..), Corecursive (..))
import Data.Map.Strict (Map)
import Control.Monad.Reader (ReaderT, MonadReader (..), runReaderT, asks)
import Control.Monad.Except (MonadError (..), Except, runExcept)
import Control.Monad.State (StateT, MonadState (..), evalStateT, gets, runStateT)
import Zipctx.Scope (AnnoExp (..), Scoped (..))
import Data.Proxy (Proxy)
import Zipctx.Structure.Stack (Stack)
import Zipctx.Structure.MonadStack (MonadStack (..))
-- import Zipctx.Structure.StackT (StackT, runStackT)
import Zipctx.Structure.StateStack (StateStackT, runStateStackT)

-- | Some monad capturing the effect of evaluation (e.g. reader, error, state, and their combinations)
type family Effect (f :: Type -> Type) :: Type -> Type

-- | A datatype expressing redexes associated with the expression functor
type family Redex (f :: Type -> Type) :: Type -> Type -> Type
-- | Intermediate state of evaluation
type family Eval (f :: Type -> Type) :: Type
-- | Unevaluated expressions
type family Uneval (f :: Type -> Type) :: Type
-- | The result of evaluation
type family Result (f :: Type -> Type) :: Type

type family Dissection (f :: Type -> Type) :: Type -> Type -> Type
type family Local (f :: Type -> Type) :: Type -> Type

-- | Machine configuration
data Elem (f :: Type -> Type) c j =
    ElemReturn !(Result f)
  | ElemFocus !(f j)
  | ElemReduce !(Redex f c j)

deriving stock instance (Eq (Result f), Eq (f j), Eq (Redex f c j)) => Eq (Elem f c j)
deriving stock instance (Show (Result f), Show (f j), Show (Redex f c j)) => Show (Elem f c j)

instance (Functor f, Bifunctor (Redex f)) => Bifunctor (Elem f) where
  bimap f g = go where
    go = \case
      ElemReturn x -> ElemReturn x
      ElemFocus fj -> ElemFocus (fmap g fj)
      ElemReduce rcj -> ElemReduce (bimap f g rcj)

-- data Config f c j = Config ![Dissection f c j] !(Elem f c j)

-- instance (Functor f, Bifunctor (Redex f), Bifunctor (Dissection f)) => Bifunctor (Config f) where
--   bimap f g (Config ds e) = Config (fmap (bimap f g) ds) (bimap f g e)

-- deriving stock instance (Eq (Result f), Eq (f j), Eq (Redex f c j), Eq (Dissection f c j)) => Eq (Config f c j)
-- deriving stock instance (Show (Result f), Show (f j), Show (Redex f c j), Show (Dissection f c j)) => Show (Config f c j)

-- | Reduction
data Reduction (f :: Type -> Type) j =
    ReductionReturn !(Result f)
  | ReductionFocus !(f j)
  deriving stock (Functor)

-- | Focus
data Focus (f :: Type -> Type) c j =
    FocusReturn !(Result f)
  | FocusDissect !j !(Dissection f c j)

data Step (f :: Type -> Type) c j =
    StepReturn !c
  -- | StepFocus !(f j)
  | StepReduce !(Redex f c j)
  | StepDissect !j !(Dissection f c j)

-- data Susp (f :: Type -> Type) c j =
--     SuspConfig !(Config f c j)
--   | SuspStep !j !(Dissection f c j)

-- deriving stock instance (Eq (Result f), Eq (f j), Eq (Redex f c j), Eq j, Eq (Dissection f c j)) => Eq (Susp f c j)
-- deriving stock instance (Show (Result f), Show (f j), Show (Redex f c j), Show j, Show (Dissection f c j)) => Show (Susp f c j)

-- instance (Functor f, Bifunctor (Redex f), Bifunctor (Dissection f)) => Bifunctor (Susp f) where
--   bimap f g = go where
--     go = \case
--       SuspConfig c -> SuspConfig (bimap f g c)
--       SuspStep j dcj -> SuspStep (g j) (bimap f g dcj)

class (Functor f, Bifunctor (Redex f), Monad (Effect f), Base (Uneval f) ~ f) => Reducible f where
  -- | Evaluates the redex
  redexReduce :: Redex f (Eval f) (Uneval f) -> Effect f (Reduction f (Uneval f))

class (Reducible f, Bifunctor (Dissection f), Base (Uneval f) ~ f) => Dissectable f where
  -- | Focus on the next element
  disFocus :: f j -> Focus f c j

  disStep :: Scoped (Local f) c j => c -> Dissection f c j -> Step f c j

  -- | Continue evaluation of the dissected expression
  -- disContinue :: Scoped (Local f) c j => c -> Dissection f c j -> Susp f c j

newtype ElemF f = ElemF { unElemF :: Elem f (Eval f) (Uneval f) }
newtype DisF f = DisF { unDisF :: Dissection f (Eval f) (Uneval f) }
type DisStack f = Stack (DisF f)

newtype MachineM f a = MachineM { unMachineM :: StateStackT (ElemF f) (DisF f) (Effect f) a }

deriving newtype instance Functor (Effect f) => Functor (MachineM f)
deriving newtype instance Monad (Effect f) => Applicative (MachineM f)
deriving newtype instance Monad (Effect f) => Monad (MachineM f)
deriving newtype instance Monad (Effect f) => MonadStack (DisF f) (MachineM f)
deriving newtype instance Monad (Effect f) => MonadState (ElemF f) (MachineM f)

eval :: (Recursive (Uneval f), Dissectable f) => Uneval f -> Effect f (ElemF f, DisStack f)
eval = error "TODO"

runMachineM :: MachineM f a -> ElemF f -> DisStack f -> Effect f (a, ElemF f, DisStack f)
runMachineM = error "TODO"

evalM :: Monad (Effect f) => MachineM f Bool
evalM = do
  ElemF elem <- get
  case elem of
    ElemReturn res -> error "TODO"
    ElemFocus jf -> error "TODO"
    ElemReduce red -> error "TODO"

onFocus :: Dissectable f => Focus f (Eval f) (Uneval f) -> MachineM f (Result f)
onFocus = \case
    FocusReturn r -> pure r
    FocusDissect j d -> error "TODO"

-- eval :: (Recursive (Uneval f), Dissectable f) => Uneval f -> MachineM f (Result f)
-- eval = onFocus . disFocus . project

-- -- Example follows:

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

type instance Effect ExpF = ExpM
type instance Result ExpF = Value
type instance Eval ExpF = Value
type instance Uneval ExpF = Exp
type instance Redex ExpF = ExpRed
type instance Dissection ExpF = ExpDis
type instance Local ExpF = ExpLocal

instance Reducible ExpF where
  redexReduce = \case
    ExpRedIf c jt je ->
      case c of
        ValueLit (LitBool b) ->
          pure (ReductionFocus (project (if b then jt else je)))
        _ -> fail "non-boolean guard"
    ExpRedApp h tl -> error "TODO"

instance Dissectable ExpF where
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

-- type ScopedExp = AnnoExp (Map Var) Value Exp

-- initExp :: Exp -> ExpF ScopedExp
-- initExp = fmap (AnnoExp mempty) . project

-- -- startEval :: ExpF ScopedExp -> ExpM Value
-- -- startEval e = do
-- --   case disStart e of
-- --     SuspReturn v -> pure v
-- --     SuspReduce rcj -> error "TODO"
-- --     SuspStep j dcj -> error "TODO"

-- -- eval :: Exp -> ExpM Value
-- -- eval = startEval . initExp

-- -- eval :: Monad m => (Redex Value -> m Value) -> Exp -> m Value
-- -- eval f = go . select where
-- --   go = \case
-- --     Left v -> pure v
-- --     Right c -> undefined
