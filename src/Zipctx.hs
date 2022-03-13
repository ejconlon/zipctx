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
-- import Control.Monad.State (StateT, MonadState, evalStateT, gets)
import Zipctx.Scope (AnnoExp (..), Scoped (..))
import Data.Proxy (Proxy)

-- | Some monad capturing the effect of evaluation (e.g. reader, error, state, and their combinations)
type family Effect (f :: Type -> Type) :: Type -> Type

-- | A datatype expressing redexes associated with the expression functor
type family Redex (f :: Type -> Type) :: Type -> Type -> Type
-- | Intermediate state of evaluation
type family Eval (f :: Type -> Type) :: Type
-- | The result of evaluation
type family Result (f :: Type -> Type) :: Type

type family Dissection (f :: Type -> Type) :: Type -> Type -> Type
type family Local (f :: Type -> Type) :: Type -> Type

-- | A step of computation
-- data Susp f r d c j =
--     SuspReturn -- (Result f)
  -- | SuspFocus !c
  -- | SuspReduce (r c j)
  -- | SuspStep !j !(Dissection f c j)

-- instance (Bifunctor r, Bifunctor d) => Bifunctor (Susp f r d x) where
--   bimap f g = go where
--     go = \case
--       SuspReturn x -> SuspReturn x
--       SuspFocus c -> SuspFocus (f c)
--       SuspReduce rcj -> SuspReduce (bimap f g rcj)
--       SuspStep j dcj -> SuspStep (g j) (bimap f g dcj)

class (Functor f, Bifunctor (Redex f), Monad (Effect f)) => Reducible f where
  -- | Evaluates the redex
  redexReduce :: Redex f (Eval f) j -> Effect f (Either (Result f) (f j))

-- type DisEffect f = Susp f (Redex f) (Dissection f) (Result f)

-- class (Reducible f, Bifunctor (Dissection f)) => Dissectable f where
--   -- type Dissection f :: Type -> Type -> Type

--   -- | Start evaluation of the expression
--   disStart :: f j -> DisEffect f c j

--   disFocus :: Eval f -> DisEffect f c j

--   -- | Continue evaluation of the dissected expression
--   disContinue :: Scoped (Local f) () j => c -> Dissection f c j -> DisEffect f c j

-- -- continue :: DisEffect f c j -> Effect f (Result f)
-- -- continue dis =
-- --     SuspReturn c -> disContinue c dis
-- --     SuspReduce re -> _
-- --     SuspStep j dis -> _

-- -- eval :: Dissectable f => f j -> Effect f (f c)
-- -- eval = continue . disStart

-- -- Example follows:

-- -- | We'll just use strings for variables.
-- -- These might include operation names like +, *, etc.
-- type Var = String

-- -- | Literals for our language
-- -- (Ignore the exclamation points when you see them, they are strictness annotations.)
-- data Lit =
--     LitInt !Int
--   | LitBool !Bool
--   deriving stock (Eq, Show)

-- -- | Values for our language
-- data Value =
--     ValueVar !Var
--   | ValueLit !Lit
--   deriving stock (Eq, Show)

-- -- | Our expression language includes vars, literals, ifs, lets, and applications
-- data Exp =
--     ExpVar !Var
--   | ExpLit !Lit
--   | ExpIf Exp Exp Exp
--   | ExpLet !Var Exp Exp
--   | ExpApp Exp !(Seq Exp)
--   deriving stock (Eq, Show)

-- -- All the ExpF/Recursive/Corecursive stuff can be derived with TemplateHaskell
-- -- but for this example it's clearer to just show everything.

-- -- | The "base functor" of our expression language.
-- -- Basically, we "punch a hole" in each constructor where they refer to recursive parts.
-- -- Note that we could take this as given and define 'Exp' as a fixpoint over this
-- -- (Something like `newtype Exp = Exp (ExpF Exp)`)
-- data ExpF a =
--     ExpVarF !Var
--   | ExpLitF !Lit
--   | ExpIfF a a a
--   | ExpLetF !Var a a
--   | ExpAppF a !(Seq a)
--   deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- -- | Boilerplate for `recursion-schemes`
-- type instance Base Exp = ExpF

-- -- | Boilerplate for `recursion-schemes`
-- instance Recursive Exp where
--   project = \case
--     ExpVar v -> ExpVarF v
--     ExpLit l -> ExpLitF l
--     ExpIf g t e -> ExpIfF g t e
--     ExpLet v b e -> ExpLetF v b e
--     ExpApp l rs -> ExpAppF l rs

-- -- | Boilerplate for `recursion-schemes`
-- instance Corecursive Exp where
--   embed = \case
--     ExpVarF v -> ExpVar v
--     ExpLitF l -> ExpLit l
--     ExpIfF g t e -> ExpIf g t e
--     ExpLetF v b e -> ExpLet v b e
--     ExpAppF l rs -> ExpApp l rs

-- -- | Redexes corresponding to our expressions.
-- -- The 'c' parts are already evaluated, and the 'j' parts are not.
-- data ExpRed c j =
--     ExpRedIf c j j
--   | ExpRedApp c !(Seq c)
--   deriving stock (Eq, Show)

-- instance Bifunctor ExpRed where
--   bimap f g = \case
--     ExpRedIf c jt je -> ExpRedIf (f c) (g jt) (g je)
--     ExpRedApp h tl -> ExpRedApp (f h) (fmap f tl)

-- -- | A 'Dissection' of our expressions.
-- -- The 'c' and 'j' parts denote evaluated/unevaluated parts as before.
-- -- We can "step" through this
-- data ExpDis c j =
--     ExpDisIf j j
--   | ExpDisLetBind !Var j
--   | ExpDisLetBody
--   | ExpDisAppHead !(Seq j)
--   | ExpDisAppTail c !(Seq c) !(Seq j)
--   deriving stock (Eq, Show)

-- instance Bifunctor ExpDis where
--   bimap f g = \case
--     ExpDisIf jt je -> ExpDisIf (g jt) (g je)
--     ExpDisLetBind v j -> ExpDisLetBind v (g j)
--     ExpDisLetBody -> ExpDisLetBody
--     ExpDisAppHead js -> ExpDisAppHead (fmap g js)
--     ExpDisAppTail c cs js -> ExpDisAppTail (f c) (fmap f cs) (fmap g js)

-- instance Reducible ExpF where
--   type Effect ExpF = ExpM
--   type Redex ExpF = ExpRed
--   type Result ExpF = Value

--   redexReduce = error "TODO"

-- data ExpLocal c = ExpLocal !Var !c
--   deriving stock (Eq, Show)

-- instance Dissectable ExpF where
--   type Dissection ExpF = ExpDis
--   type Local ExpF = ExpLocal

--   disStart = \case
--     ExpVarF v -> SuspReturn (ValueVar v)
--     ExpLitF l -> SuspReturn (ValueLit l)
--     ExpIfF g t e -> SuspStep g (ExpDisIf t e)
--     ExpLetF v e b -> SuspStep e (ExpDisLetBind v b)
--     ExpAppF h tl -> SuspStep h (ExpDisAppHead tl)

--   disContinue c = \case
--     ExpDisLetBind v jb -> SuspStep (localScope (ExpLocal v c) jb) ExpDisLetBody
--     ExpDisLetBody -> SuspFocus c
--     ExpDisIf jt je -> SuspReduce (ExpRedIf c jt je)
--     ExpDisAppHead jtl ->
--       case jtl of
--         Empty -> SuspReduce (ExpRedApp c Empty)
--         j :<| jtl' -> SuspStep j (ExpDisAppTail c Empty jtl')
--     ExpDisAppTail chd ctl jtl ->
--       case jtl of
--         Empty -> SuspReduce (ExpRedApp chd (ctl :|> c))
--         j :<| jtl' -> SuspStep j (ExpDisAppTail c (ctl :|> c) jtl')

-- type Error = String
-- type VarEnv = Map Var Value

-- newtype ExpM a = ExpM { unExpM :: ReaderT VarEnv (Except Error) a }
--   deriving newtype (Functor, Applicative, Monad, MonadReader VarEnv, MonadError Error)

-- runExpM :: ExpM a -> VarEnv -> Either Error a
-- runExpM m ve = runExcept (runReaderT (unExpM m) ve)

-- instance MonadFail ExpM where
--   fail = throwError

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
