module TypeChecker.Subst where

import CoalTerm
import Data.List
import Control.Monad (unless)

import Debug.Trace

type Unifier = [(String,Type)]

type UnificationError = String

class Subst t where
  apply :: Unifier -> t -> t
  tv :: t -> [String]

(|->) :: String -> Type -> Unifier
a |-> ty = [(a,ty)]

instance Subst Type where
  apply u ty1 =
    case ty1 of
      TyVar x -> case lookup x u of
                   Nothing -> TyVar x
                   Just ty -> ty
      ProdTy t1 t2 -> ProdTy (apply u t1) (apply u t2)
      FunTy t1 t2 -> FunTy (apply u t1)  (apply u t2)
      t -> t
  tv ty =
    case ty of
      TyVar x -> [x]
      ProdTy ty1 ty2 -> tv ty1 `union` tv ty2
      FunTy ty1 ty2 -> tv ty1 `union` tv ty2
      _ -> []

instance Subst a => Subst [a] where
  apply u = map (apply u)
  tv = nub . concatMap tv

instance (Subst a, Subst b) => Subst (a,b) where
  apply u (a,b) = (apply u a, apply u b)
  tv (a,b) = tv a `union` tv b

infixr 4 @@
-- apply (u1 @@ u2) == apply u1 . apply u2
(@@) :: Unifier -> Unifier -> Unifier
u1 @@ u2 = [(a, apply u1 b) | (a,b) <- u2] ++ u1

-- apply (u1 `merge` u2) == apply (u2 `merge` u1)  (modulo Nothing)
merge :: Unifier -> Unifier -> Either UnificationError Unifier
merge u1 u2 =
  let dom = map fst
      compatible = all (\v -> apply u1 (TyVar v) == apply u2 (TyVar v))
                   (dom u1 `intersect` dom u2)
      u = u1 ++ u2
  in if compatible then Right u else Left "incompatible unifiers"

failsOccursCheck :: String -> Type -> Bool
failsOccursCheck a ty = a `elem` tv ty

varSubst :: String -> Type -> Either UnificationError Unifier
varSubst a ty
  | ty == TyVar a = return []
  | failsOccursCheck a ty = Left "occurs check failed"
  | otherwise = return (a |-> ty)

mguSet :: [(Type,Type)] -> Either UnificationError Unifier
mguSet [] = return []
mguSet ((ty1,ty2):cs) =
  do u1 <- mgu ty1 ty2
     u2 <- mguSet (apply u1 cs)
     return (u2 @@ u1)
  
mgu :: Type -> Type -> Either UnificationError Unifier
mgu (FunTy ty1 ty2) (FunTy ty3 ty4) =
  mguSet [(ty1,ty3),(ty2,ty4)]
  {-  do u1 <- mgu ty1 ty3
     u2 <- mgu (apply u1 ty2) (apply u1 ty4)
     return (u2 @@ u1)-} 
mgu (ProdTy ty1 ty2) (ProdTy ty3 ty4) = mguSet [(ty1,ty3),(ty2,ty4)]
mgu (TyVar a) ty2 = varSubst a ty2
mgu ty1 (TyVar a) = varSubst a ty1
mgu ty1 ty2 | ty1 == ty2 = return []
            | otherwise = Left "no unifier exists"
