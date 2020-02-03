{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Eml.TypeChecker where

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.Fresh
import           Control.Effect.Lift


import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe

import qualified Language.Eml.AST      as A
import           Language.Eml.Operator

import           Language.Eml.Type

-- Substitution map
type Subst = Map String Type

freshTyVar ::
  ( Member Fresh sig
  , Carrier sig m
  ) => m String
freshTyVar = ("t"<>) . show <$> fresh

applySubst :: Subst -> Type -> Type
applySubst _ NumType        = NumType
applySubst _ BoolType       = BoolType
applySubst _ StringType     = StringType
applySubst _ UnitType       = UnitType
applySubst s (TyForall _ t) = applySubst s t -- very iffy
applySubst s (a :~> b)      = applySubst s a :~> applySubst s b
applySubst s (TyVar v)      = fromMaybe (TyVar v) $ Map.lookup v s

composeSubsts :: Subst -> Subst -> Subst
composeSubsts s f = f <> (applySubst f <$> s)


-- Check whether type var occurs in type
occurs :: String -> Type -> Bool
occurs v (TyVar v')     = v == v'
occurs _ NumType        = False
occurs _ BoolType       = False
occurs _ StringType     = False
occurs _ UnitType       = False
occurs _ (TyForall _ _) = False -- iffy
occurs v (a :~> b)      = occurs v a || occurs v b

unify :: ( Member (Error TypeError) sig
         , Carrier sig m
         ) => Type -> Type -> m Subst
unify t1 t2                   | t1 == t2 = pure Map.empty
unify (TyVar v) tv@(TyVar _) = pure (Map.singleton v tv)
unify (TyVar v) t2
  | occurs v t2 = throwError $ OccursCheck v t2
  | otherwise = pure (Map.singleton v t2)
unify (a :~> b) (c :~> d) = do
  s1 <- unify a c
  s2 <- unify (applySubst s1 b) (applySubst s1 d)
  pure $ composeSubsts s1 s2
-- unify NumType (TyVar v) = pure (Map.singleton v NumType) -- iffy
-- unify BoolType (TyVar v) = pure (Map.singleton v BoolType) -- iffy
-- unify StringType (TyVar v) = pure (Map.singleton v StringType) -- iffy
-- unify UnitType (TyVar v) = pure (Map.singleton v UnitType) -- iffy
unify t1 t2 = throwError $ UnificationError t1 t2

instantiate ::
  ( Member Fresh sig
  , Carrier sig m
  ) => Type -> m Type
instantiate (TyForall n ty) = do
  freshTy <- freshTyVar
  applySubst (Map.singleton n (TyVar freshTy)) <$> instantiate ty
instantiate (a :~> b) = (:~>) <$> instantiate a <*> instantiate b
instantiate NumType = pure NumType
instantiate BoolType = pure BoolType
instantiate StringType = pure StringType
instantiate UnitType = pure UnitType
instantiate (TyVar tv) = pure (TyVar tv)

-- add forall to all free variables
generalize :: Type -> Type
generalize ty = foldr TyForall ty (ftv ty)


-- runCheck :: forall m a sig.
--   ( Member Fresh sig
--   , Member (Error TypeError) sig
--   , Carrier sig m ) => m a -> IO (Either TypeError a)
runCheck m = runM . runError . runFresh $ m

inferIO :: Map String Type -> A.Expr -> IO (Either TypeError (Subst, Type))
inferIO = fmap runCheck . infer

checkIO :: A.Module -> IO (Either TypeError (Map String Type))
checkIO = runCheck . checkModule

unifyIO :: Type -> Type -> IO (Either TypeError Subst)
unifyIO a b = runCheck $ unify a b
-- unify :: ( Member (Error TypeError) sig
--          , Carrier sig m
--          ) => Type -> Type -> m Subst
checkModule ::
  ( Member Fresh sig
  , Member (Error TypeError) sig
  , Carrier sig m ) => A.Module -> m (Map String Type)
checkModule (A.Module _ bindings) = go (Map.singleton "eval" (StringType :~> TyForall "a" (TyVar "a"))) bindings
  where go env (A.Definition name expr : bindings') = do
          (_, t) <- infer env expr
          go (Map.insert name (generalize t) env) bindings'
        go env [] = pure env

infer ::
  ( Member Fresh sig
  , Member (Error TypeError) sig
  , Carrier sig m ) => Map String Type -> A.Expr -> m (Subst, Type)
infer env expr = case expr of
  A.Var v -> case Map.lookup v env of
    Nothing -> throwError $ MissingVariable v
    Just ty -> pure (Map.empty, ty)
  A.NumLit _ -> pure (Map.empty, NumType)
  A.StringLit _ -> pure (Map.empty, StringType)
  A.Asc e t -> do
    (es, et) <- infer env e
    ss <- unify et t
    pure (composeSubsts ss es, t)
  A.If c t f -> do
    (cs, ct) <- infer env c
    tvar <- freshTyVar
    (ts, tt) <- infer env t
    (fs, ft) <- infer env f
    bs <- unify ct BoolType
    ct' <- instantiate $ applySubst bs ct
    tt' <- instantiate $ applySubst (composeSubsts cs bs) tt
    ft' <- instantiate $ applySubst (foldr composeSubsts Map.empty [ts, cs, bs]) ft
    ss <- unify (BoolType :~> TyVar tvar :~> TyVar tvar) (ct' :~> tt' :~> ft')
    let sss = foldr composeSubsts Map.empty [ss, fs, ts, cs, bs]
    pure (sss, applySubst sss (TyVar tvar))
  A.BinOp op l r -> do
    opTy <- inferOp op
    (ls, lt) <- infer env l
    (rs, rt) <- infer env r
    tvar <- freshTyVar
    lt' <- instantiate lt
    rt' <- instantiate rt
    ss <- unify (lt' :~> applySubst ls rt' :~> TyVar tvar) opTy
    let sss = foldr composeSubsts Map.empty [ss, rs, ls]
    pure (sss, applySubst sss (TyVar tvar))
  A.App fun arg -> do
    tvar <- freshTyVar
    (fs, ft) <- infer env fun
    (as, at) <- infer env arg
    ft' <- instantiate ft
    at' <- instantiate (applySubst fs at)
    ss <- unify (at' :~> TyVar tvar) ft'
    let sss = foldr composeSubsts Map.empty [fs, as, ss]
    pure (sss, applySubst sss (TyVar tvar))
  A.Lam param body -> do
    tvar <- freshTyVar
    (bs, bt) <- infer (Map.insert param (TyVar tvar) env) body
    bt' <- instantiate bt
    pure (bs, applySubst bs (TyVar tvar) :~> bt')
  A.Let name rhs body -> do
    tvar <- freshTyVar
    (rs, rt) <- infer (Map.insert name (TyVar tvar) env) rhs
    rss <- unify (TyVar tvar) rt
    let rt' = generalizeIfValue rhs (applySubst rss (TyVar tvar))
    (bs, bt) <- infer (Map.insert name rt' env) body
    pure (composeSubsts bs rs, applySubst rs bt)


  where
    generalizeIfValue e ty
      | isValue e = generalize ty
      | otherwise = ty

    isValue :: A.Expr -> Bool
    isValue expr' = case expr' of
      (A.Let _ r b)   -> isValue r && isValue b
      (A.Lam _ _)     -> True
      (A.App _ _)     -> False
      (A.BinOp _ _ _) -> False
      (A.If _ _ _)    -> False
      (A.Asc e _)     -> isValue e
      (A.NumLit _)    -> True
      (A.StringLit _) -> True
      (A.Var _)       -> False

    inferOp Plus     = pure (NumType :~> NumType :~> NumType)
    inferOp Minus    = pure (NumType :~> NumType :~> NumType)
    inferOp Multiply = pure (NumType :~> NumType :~> NumType)
    inferOp _ = do -- This may be really bad
      freshTy <- freshTyVar
      pure (TyVar freshTy :~> TyVar freshTy :~> TyVar freshTy)


