{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Unify where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Map hiding (foldl, map)
import qualified Data.Map as M

{-
 -- What I incorrectly expected. But let's not do that
instance Monad (Either String) where
  return = Right
  Left  l >>= _ = Left l
  Right r >>= k = k r
  fail = Left
-}

newtype Name = N String deriving (Eq, Ord, Show)
data Type
  = TypeVar Name
  | Fun Type Type
  deriving (Show, Eq)

-- "Smart" constructors
tVar :: String -> Type
tVar = TypeVar . N

fun :: Type -> Type -> Type
fun = Fun

compose :: Subst -> Subst -> Subst
doSubst :: Subst -> Type -> Type
singletonSubst :: Name -> Type -> Subst
emptySubst :: Subst

{-
newtype Subst = Subst { substImpl :: Name -> Type }

compose s1 s2 = Subst $ doSubst s1 . substImpl s2

emptySubst = Subst $ TypeVar
singletonSubst n t = Subst $ \n2 -> if n2 == n then t else TypeVar $ n2

doSubst (Subst map) (TypeVar n) = map n
doSubst s (Fun t u) = Fun (doSubst s t) (doSubst s u)
-}

newtype Subst = Subst { unSubst :: Map Name Type }
  deriving (Monoid, Show, Eq)

--compose = mappend --Wrong!
-- This is s1 (s2 x).
-- We use flip mappend because we don't want to lose the entries from the right.
compose s1 s2 = Subst . (flip mappend $ unSubst s1) . M.map (doSubst s1) . unSubst $ s2

-- Of course, compose is much easier if substitutions are functions! But then we
-- can't show substitutions, which is rather annoying.

doSubst (Subst map) (TypeVar n) = fromMaybe (TypeVar n) (M.lookup n map)
doSubst s (Fun t u) = Fun (doSubst s t) (doSubst s u)

emptySubst = Subst mempty
singletonSubst n t = Subst $ insert n t mempty

freeVars :: Type -> [Name]
freeVars (TypeVar n) = pure n
freeVars (Fun t u) = freeVars t <|> freeVars u

-- Based on
-- http://www.dmi.unict.it/~barba/LinguaggiII.html/READING_MATERIAL/LAMBDACALCULUS/LAMBDACALCULUS.1.HTM
unify :: Type -> Type -> Either String Subst
unify (TypeVar n) tau | not (n `elem` freeVars tau) = Right $ singletonSubst n tau
unify (TypeVar n) (TypeVar n1) | n == n1 = Right $ emptySubst
unify t1 t2@(TypeVar n) = unify t2 t1
unify (Fun t1 u1) (Fun t2 u2) = do
  s1 <- unify t1 t2
  s2 <- unify (s1 `doSubst` u1) (s1 `doSubst` u2)
  return $ s2 `compose` s1

unify _ _ = Left "unification failure"

successful :: Either a b -> Bool
successful (Left _) = False
successful (Right _) = True

-- vim: set sw=2:
