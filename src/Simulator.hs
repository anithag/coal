{-# language GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# language DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Simulator where

import CoalTerm hiding (get)
import Data.Map as M
import Data.List as L
import Control.Monad.Trans
import Control.Monad.State.Lazy
import qualified Data.Binary as B
import Data.Typeable
import GHC.Generics (Generic)


data ThreadCfg =
  Cfg { term :: Term,
        env  :: M.Map String Term
  }

--newtype CoalSim a = CoalSim { unCoalSim :: Maybe a }
--   deriving (Functor, Applicative, Monad, MonadIO)

--instance SimulatorM CoalSim where

eval :: ThreadCfg -> Maybe ThreadCfg
eval cfg = case (term cfg) of
  Var x -> let tenv = env cfg in
    case (M.lookup x tenv) of
      Nothing -> fail $ "Lookup for variable " ++ x ++ " failed."
      Just t ->  return cfg{ term = t }
  I n ->  return cfg
  Unit ->  return cfg
  Abs x ty t ->  return cfg
  InjL t ty -> do
    cfg' <- eval cfg{term = t}
    return cfg{term = InjL (term cfg') ty }
  InjR t ty -> do
    cfg' <- eval cfg{term = t}
    return cfg{term = InjR (term cfg') ty } 
  App t1 t2 -> do 
    lamcfg <- eval cfg{term = t1}
    argcfg <- eval cfg{term = t2}
    case (term lamcfg) of
      Abs x ty  t -> do
        let e' = env lamcfg
        let v = term argcfg
        return lamcfg{term = t, env = M.insert x v e'}
  Case t1 x t2 y t3 -> do
    ccfg <- eval cfg{term = t1}
    let e' = env cfg
    case (term ccfg) of
      InjL  v _ -> eval cfg{term = t2, env = M.insert x v e'}
      InjR  v _ -> eval cfg{term = t3, env = M.insert y v e'}
  Pair t1 t2 -> do
    cfg1 <- eval cfg{term = t1}
    cfg2 <- eval cfg{term = t2}
    return cfg{term = Pair (term cfg1) (term cfg2)}
  Fst t -> do
    cfg' <- eval cfg{term = t}
    case (term cfg') of
      Pair v1 v2 -> return cfg{term =v1}
      _ -> fail $ "Expected a pair value, but received none"
  Snd t -> do
    cfg' <- eval cfg{term = t}
    case (term cfg') of
      Pair v1 v2 -> return cfg{term =v2}
      _ -> fail $ "Expected a pair value, but received none"
  Bind x t1 t2  -> do
    cfg1 <- eval cfg{term = t1}
    let e' = env cfg
    case (term cfg1) of
      Protect l t -> return cfg{term = t2, env = M.insert x t e'}
      _ -> fail $ "Expected a protected value, received none"
  Protect l t  -> do
     cfg' <- eval cfg{term = t}
     return cfg{term = Protect l (term cfg')} -- do the encryption/signing here?
  _ -> fail  $ "Case not handled"

