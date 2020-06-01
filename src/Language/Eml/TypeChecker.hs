{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Eml.TypeChecker where

import Control.Effect
import Control.Effect.Error
import Control.Effect.Fresh
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
-- REMOVE
import Debug.Trace
import qualified Language.Eml.AST as A
import Language.Eml.Operator
import Language.Eml.Type as Type

-- Substitution map
type Subst = Map String Type

freshTyVar ::
  ( Member Fresh sig,
    Carrier sig m
  ) =>
  m String
freshTyVar = ("t" <>) . show <$> fresh

applyS :: Subst -> Type -> Type
applyS _ t@(TyCon _) = t
applyS s (TyForall f t) = TyForall f (applyS s t) -- very iffy
applyS s (a :~> b) = applyS s a :~> applyS s b
applyS s (TyVar v) = fromMaybe (TyVar v) $ Map.lookup v s

composeS :: Subst -> Subst -> Subst
composeS s f = f <> (applyS f <$> s)

flattenS :: [Subst] -> Subst
flattenS = foldr composeS Map.empty

-- Check whether type var occurs in type
occurs :: String -> Type -> Bool
occurs v (TyVar v') = v == v'
occurs _ (TyCon _) = False
occurs _ (TyForall _ _) = False -- iffy
occurs v (a :~> b) = occurs v a || occurs v b

unify ::
  ( Member (Error TypeError) sig,
    Carrier sig m
  ) =>
  Type ->
  Type ->
  m Subst
unify t1 t2 | t1 == t2 = pure Map.empty
unify (TyVar v) tv@(TyVar _) = pure (Map.singleton v tv)
unify (TyVar v) t2
  | occurs v t2 = throwError $ OccursCheck v t2
  | otherwise = pure (Map.singleton v t2)
unify t1 (TyVar v)
  | occurs v t1 = throwError $ OccursCheck v t1
  | otherwise = pure (Map.singleton v t1)
unify (a :~> b) (c :~> d) = do
  s1 <- unify a c
  s2 <- unify (applyS s1 b) (applyS s1 d)
  pure $ s1 `composeS` s2
-- unify NumType (TyVar v) = pure (Map.singleton v NumType) -- iffy
-- unify BoolType (TyVar v) = pure (Map.singleton v BoolType) -- iffy
-- unify StringType (TyVar v) = pure (Map.singleton v StringType) -- iffy
-- unify UnitType (TyVar v) = pure (Map.singleton v UnitType) -- iffy
unify t1 t2 = throwError $ UnificationError t1 t2

instantiate ::
  ( Member Fresh sig,
    Carrier sig m
  ) =>
  Type ->
  m Type
instantiate (TyForall n ty) = do
  freshTy <- freshTyVar
  applyS (Map.singleton n (TyVar freshTy)) <$> instantiate ty
instantiate (a :~> b) = (:~>) <$> instantiate a <*> instantiate b
instantiate t@(TyCon _) = pure t
instantiate (TyVar tv) = pure (TyVar tv)

-- add forall to all free variables
generalize :: Type -> Type
generalize ty = foldr TyForall ty (ftv ty)

runCheck m = runM . runError . runFresh $ m

inferIO :: Map String Type -> A.Expr -> IO (Either TypeError (Subst, Type))
inferIO = fmap runCheck . infer

checkIO :: A.Module -> IO (Either TypeError (Map String Type))
checkIO = runCheck . checkModule

unifyIO :: Type -> Type -> IO (Either TypeError Subst)
unifyIO a b = runCheck $ unify a b

standardEnv :: Map String Type
standardEnv =
  Map.fromList
    [("eval", TyForall "a" (Type.stringTy :~> TyVar "a"))]

checkModule ::
  ( Member Fresh sig,
    Member (Error TypeError) sig,
    Carrier sig m
  ) =>
  A.Module ->
  m (Map String Type)
checkModule (A.Module _ bindings) = go standardEnv bindings
  where
    go env (A.Definition name expr : bindings') = do
      tvar <- freshTyVar
      (_, rt) <- infer (Map.insert name (TyVar tvar) env) expr
      rss <- unify (TyVar tvar) rt
      let ft = generalizeIfValue expr $ applyS rss (TyVar tvar)
      go (Map.insert name ft env) bindings'
    go env [] = pure env

{- This function is responsible for ensuring your ascription is strictly unifiable in one direction, namely, you are able to restrict types, but not extend them.
   This function needs verification.
-}
ascCheck ::
  ( Member Fresh sig,
    Member (Error TypeError) sig,
    Carrier sig m
  ) =>
  Map String Type ->
  A.Expr ->
  Type ->
  m Type
ascCheck env expr t = do
  (s, t') <- infer env expr
  ss <- unify t' t
  let ut = applyS (s `composeS` ss) t
  if ut == t
    then pure t
    else throwError $ UnificationError t t'

infer ::
  ( Member Fresh sig,
    Member (Error TypeError) sig,
    Carrier sig m
  ) =>
  Map String Type ->
  A.Expr ->
  m (Subst, Type)
infer env expr = case expr of
  A.Var v -> case Map.lookup v env of
    Nothing -> throwError $ MissingVariable v
    Just ty -> pure (Map.empty, ty)
  A.NumLit _ -> pure (Map.empty, Type.numTy)
  A.StringLit _ -> pure (Map.empty, Type.stringTy)
  A.Asc e t -> do
    rt <- ascCheck env e t
    pure (Map.empty, rt)
  A.If c t f -> do
    (cs, ct) <- infer env c
    (ts, tt) <- infer env t
    (fs, ft) <- infer env f
    tvar <- freshTyVar
    bs <- unify ct Type.boolTy
    ct' <- instantiate $ applyS bs ct
    tt' <- instantiate $ applyS (cs `composeS` bs) tt
    ft' <- instantiate $ applyS (foldr composeS Map.empty [ts, cs, bs]) ft
    ss <- unify (Type.boolTy :~> TyVar tvar :~> TyVar tvar) (ct' :~> tt' :~> ft')
    let sss = flattenS [ss, fs, ts, cs, bs]
    pure (sss, applyS sss (TyVar tvar))
  A.BinOp op l r -> do
    opTy <- inferOp op
    (ls, lt) <- infer env l
    (rs, rt) <- infer env r
    tvar <- freshTyVar
    lt' <- instantiate lt
    rt' <- instantiate rt
    ss <- unify (lt' :~> applyS ls rt' :~> TyVar tvar) opTy
    let sss = flattenS [ss, rs, ls]
    pure (sss, applyS sss (TyVar tvar))
  A.App fun arg -> do
    tvar <- freshTyVar
    (fs, ft) <- infer env fun
    (as, at) <- infer env arg
    ft' <- instantiate ft
    at' <- instantiate (applyS fs at)
    ss <- unify (at' :~> TyVar tvar) ft'
    let sss = flattenS [fs, as, ss]
    pure (sss, applyS sss (TyVar tvar))
  A.Lam param body -> do
    tvar <- freshTyVar
    (bs, bt) <- infer (Map.insert param (TyVar tvar) env) body
    bt' <- instantiate bt
    pure (bs, applyS bs (TyVar tvar) :~> bt')
  A.Let name rhs body -> do
    tvar <- freshTyVar
    (rs, rt) <- infer (Map.insert name (TyVar tvar) env) rhs
    rss <- unify (TyVar tvar) rt
    let rt' = generalizeIfValue rhs (applyS rss (TyVar tvar))
    (bs, bt) <- infer (Map.insert name rt' env) body
    pure (bs `composeS` rs, applyS rs bt)
  where
    inferOp Plus = pure (Type.numTy :~> Type.numTy :~> Type.numTy)
    inferOp Minus = pure (Type.numTy :~> Type.numTy :~> Type.numTy)
    inferOp Multiply = pure (Type.numTy :~> Type.numTy :~> Type.numTy)
    inferOp _ = do
      -- This may be really bad
      freshTy <- freshTyVar
      pure (TyVar freshTy :~> TyVar freshTy :~> TyVar freshTy)

generalizeIfValue :: A.Expr -> Type -> Type
generalizeIfValue e ty
  | isValue e = generalize ty
  | otherwise = ty

isValue :: A.Expr -> Bool
isValue expr' = case expr' of
  (A.Let _ r b) -> isValue r && isValue b
  (A.Lam _ _) -> True
  (A.App _ _) -> False
  A.BinOp {} -> False
  A.If {} -> False
  (A.Asc e _) -> isValue e
  (A.NumLit _) -> True
  (A.StringLit _) -> True
  (A.Var _) -> False
