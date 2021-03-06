{-# language GeneralizedNewtypeDeriving #-}
module TypeChecker.Monad where

import FreshM
import qualified Data.Map as M
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Except as E
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Except
import Control.Monad (unless)
import Debug.Trace

import CoalTerm
import TypeChecker.Subst

data TcError = TcError String
type TyEnv = (M.Map String Type, M.Map String Type)

newtype TcM a = TcM { unTcM :: E.ExceptT TcError (S.StateT Unifier FreshM) a }
  deriving (Functor, Applicative, Monad,
            MonadState Unifier, MonadError TcError)

runTcM :: TcM a -> (Either TcError a, Unifier)
runTcM m = runFresh (S.runStateT (E.runExceptT (unTcM m)) [])

instance MonadFresh TcM where
  fresh = TcM $ lift  (lift fresh)

lookupTy :: TyEnv -> String -> TcM Type
lookupTy (gamma,_) x =
  case M.lookup x gamma of
    Nothing -> throwError $ TcError $ "unbound variable " ++ x
    Just ty -> return ty

lookupResTy :: TyEnv -> String -> TcM Type
lookupResTy (_,resenv) x =
  case M.lookup x resenv of
    Nothing -> throwError $ TcError $ "unknown resource " ++ x
    Just ty -> return ty

extend :: String -> Type -> TyEnv -> TyEnv
extend x ty (gamma, resenv) =
  (M.insert x ty gamma, resenv)

unify :: Type -> Type -> TcM ()
unify ty1 ty2 =
  do u <- get
     case mgu (apply u ty1) (apply u ty2) of
       Left err -> throwError $ TcError $ "Type error: " ++ err
       Right u' -> modify (u'@@)

guard :: Bool -> String -> TcM ()
guard b errMsg =
  if b
  then return ()
  else throwError $ TcError $ errMsg

getUnifier :: TcM Unifier
getUnifier = get

withErrCtxt :: String -> TcM a -> TcM a
withErrCtxt errMsg (TcM m) =
  TcM $ withExceptT (\(TcError s) -> TcError (s ++ errMsg)) m

instance Show TcError where
  show (TcError str) = str
