module TypeChecker where

import qualified Data.Map as M

import CoalTerm
import TypeChecker.Monad
import TypeChecker.Subst
import FreshM
import Control.Monad (when)

unifyMsg :: Term -> Type -> Type -> String
unifyMsg e ty1 ty2 =
  "\n while trying to unify types of: " ++ show e ++ "\n\t" ++
  "Inferred type: " ++ show ty1 ++ "\n\t" ++
  "Expected type: " ++ show ty2

unifyWithCtxt :: Term -> Type -> Type -> TcM ()
unifyWithCtxt e ty1 ty2 =
  do u <- getUnifier
     unifyWithMsg (unifyMsg e (apply u ty1) (apply u ty2)) ty1 ty2
  
unifyWithMsg :: String -> Type -> Type -> TcM ()
unifyWithMsg str ty1 ty2 =
  withErrCtxt str $ unify ty1 ty2

tc :: TyEnv -> Term -> TcM (Type, TyEnv)
tc gamma t =
 withErrCtxt ("\n in " ++ show t) $
  case t of
    Var x -> do
      ty <- lookupTy gamma x
      return (ty, gamma)
    App e1 e2 -> do
      (ty, g1) <- tc gamma e1
      (ty3, g2) <- tc gamma e2
      a <- fresh
      b <- unifyWithCtxt e1 ty (FunTy "x" ty3 (TyVar a)) -- Fix this for dependent types                   
      return (TyVar a,  (M.union (fst g1) (fst g2), M.union (snd g1) (snd g2)))
    Abs x ty e -> do
                  (ty', g') <- tc (extend x ty gamma) e
                  return (FunTy x ty ty', g')
    CTrue -> return (BoolTy, gamma)
    CFalse -> return (BoolTy, gamma)
    I _ -> return (IntTy, gamma)
    Unit -> return (UnitTy, gamma)
    Case c x e1 y e2 ->
      do (condTy, gc) <- tc gamma c
         unifyWithCtxt c condTy BoolTy
         (ty1, g1) <-  tc gamma e1
         (ty2, g2) <-  tc gamma e2
         unify ty1 ty2
         return (ty1,  (M.union (fst g1) (fst g2), M.union (snd g1) (snd g2)))
    Fst e ->
      do (ty, g') <- tc gamma e
         a <- fresh
         b <- fresh
         unifyWithCtxt e ty (ProdTy (TyVar a) (TyVar b))
         return (TyVar a, g')
    Snd e ->
      do (ty, g') <- tc gamma e
         a <- fresh
         b <- fresh
         unifyWithCtxt e ty (ProdTy (TyVar a) (TyVar b))
         return (TyVar b, g')
    Pair e1 e2 ->
      do (ty1, g1) <- tc gamma e1
         (ty2, g2) <-  tc gamma e2
         return (ProdTy ty1 ty2,  (M.union (fst g1) (fst g2), M.union (snd g1) (snd g2)))
    Nil -> do a <- fresh
              return (ListTy (TyVar a), gamma)
    Cons t1 t2 ->
      do (ty1, g1) <- tc gamma t1
         (ty2, g2) <- tc gamma t2
         a <- fresh
         unify ty2 (ListTy (TyVar a))
         unify ty1 (TyVar a)
         return (ListTy (TyVar a),   (M.union (fst g1) (fst g2), M.union (snd g1) (snd g2)))

    EBPFAdd -> return (EBPFTy, gamma)
    EBPFSub -> return (EBPFTy, gamma)
    EBPFMov -> return (EBPFTy, gamma)
    EBPFJmp -> return (EBPFTy, gamma)

typeCheckWith :: TyEnv -> Term -> IO ()
typeCheckWith tyEnv t =
  let (ety, u) = runTcM (tc tyEnv t)
  in case ety of
    Left err -> print err
    Right (ty, gamma) -> do putStr "Principal type: "
                            print (apply u ty)

typeCheck :: Term -> IO ()
typeCheck = typeCheckWith (M.empty, M.empty)

testTc :: Term -> (Either TcError (Type, TyEnv), Unifier)
testTc t =
  let tyEnv = (M.empty, M.empty)
  in runTcM (tc tyEnv t)

basicEnv :: TyEnv
basicEnv = (M.empty, M.fromList [])
