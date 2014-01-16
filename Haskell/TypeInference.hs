{-# LANGUAGE DeriveDataTypeable #-}
module TypeInference where

import Unify
import Debug.Trace

-- Imports copied from Unify
import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.Map hiding (foldl, map, null)
import Data.Generics
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

-- Typing contexts:
type Context = M.Map Name Type

emptyCtx :: Context
emptyCtx = M.empty

pushCtx :: Name -> Type -> Context -> Context
pushCtx = M.insert

lookupCtx :: Name -> Context -> Maybe Type
lookupCtx = M.lookup

contextEqs :: Context -> Context -> [(Type, Type)]
contextEqs = (elems .) . M.intersectionWith (=:=)

data Term
  = Var Name
  | App Term Term
  | Lam Name Term
  deriving (Show, Eq)

var = Var . N
{-
-- Typed Term
data TTerm
  = TVar Name Type
  | TApp TTerm TTerm
  | TLam Name Type TTerm

data TypingJudgment = Judg Context TTerm Type
-}

-- Typing judgments contain typed terms, which in turn contain not just subterms
-- but typing derivations for them!
data TypingJudgment
  = Judgment
  { context :: Context
  , tterm :: TTerm
  , typ :: Type
  }
  deriving (Show, Eq, Data, Typeable)

-- Typed Term. Includes the full typing judgment!
data TTerm
  = TVar Name
  | TApp TypingJudgment TypingJudgment
  | TLam Name Type TypingJudgment
  deriving (Show, Eq, Data, Typeable)

type TInferM a = State Int a

fresh :: TInferM Name
fresh = state (\counter -> (N $ "x_" ++ show counter, counter + 1))

freshTVar :: TInferM Type
freshTVar = TypeVar <$> fresh

-- Non-constraint-based type inference, taken from slides by Peter Thiemann and
-- Manuel Geffken:
-- https://proglang.informatik.uni-freiburg.de/teaching/compilerbau/2012ws/17-simply-typed.pdf
-- It is not constraint-based because unification is not done in one big swoop
-- at the end, but interleaved in between.
--
-- Moreover, in this description of type inference, the context is an output,
-- not an input of type inference.
--
-- In this implementation, the freshness condition are expressed through a stateful generator of fresh variables.
-- Warning: bugs are expected.
typeInfer :: Term -> TInferM TypingJudgment

typeInfer (Var name) = do
  tVar <- freshTVar
  let ctx = pushCtx name tVar emptyCtx
  return $ Judgment ctx (TVar name) tVar

typeInfer (Lam varName body) = do
  bodyTJudg @ (Judgment ctx tBody bodyType) <- typeInfer body
  case lookupCtx varName ctx of
    Just varType ->
      return $ Judgment ctx (TLam varName varType bodyTJudg) (fun varType bodyType)
    Nothing -> do
      newTVar <- freshTVar
      return $ Judgment (pushCtx varName newTVar ctx) (TLam varName newTVar bodyTJudg) (fun newTVar bodyType)

typeInfer (App fun arg) = do
  funTJudg @ (Judgment funCtx funTerm funType) <- typeInfer fun
  argTJudg @ (Judgment argCtx argTerm argType) <- typeInfer arg
  newTVar <- assert (disjointTVars funTJudg argTJudg) $ freshTVar
  let (Right subst) = unify_eqs (traceOne ((funType =:= argType =:> newTVar) : contextEqs funCtx argCtx))
  let mergedContext = contextSubst (traceOne subst) funCtx `union` contextSubst subst argCtx
  return $ Judgment mergedContext (ttermSubst subst (TApp funTJudg argTJudg)) (doSubst subst newTVar)

getTVarsJudg = everything (++) (mkQ [] getTVars)

traceOne a = trace ('\n' : show a) a
--traceOne a = a

disjointTVars :: TypingJudgment -> TypingJudgment -> Bool
disjointTVars aJudg @ (Judgment aCtx aTerm aType) bJudg @ (Judgment bCtx bTerm bType) =
  null $ traceOne (getTVarsJudg aJudg) `L.intersect` traceOne (getTVarsJudg bJudg)

getTVars :: Type -> [Name]
getTVars (TypeVar n) = [n]
getTVars (Fun a b) = getTVars a ++ getTVars b

contextSubst subst ctx = M.map (doSubst subst) ctx

{-
data TypingJudgment
  = Judgment
  { context :: Context
  , tterm :: TTerm
  , typ :: Type
  }
  deriving (Show, Eq)
-}

ttermSubst subst term = termSub term
  where
    termSub (TApp fun arg) = TApp (judgSub subst fun) (judgSub subst arg)
    termSub (TLam name typ body) = TLam name (doSubst subst typ) (judgSub subst body)
    termSub other = other

judgSub subst (Judgment ctx tterm typ) = Judgment (contextSubst subst ctx) (ttermSubst subst tterm) (doSubst subst typ)

-- vim: set sw=2:
