{-# language GeneralizedNewtypeDeriving, FlexibleContexts #-}
module FreshM where

import Control.Monad.State
import Control.Monad.Trans

newtype FreshM b =
  FreshM { runFreshM :: State Int b }
  deriving (Functor, Applicative, Monad)

newtype FreshT m b =
  FreshT { unFreshT :: StateT Int m b }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class (Monad m) => MonadFresh m where
  fresh :: m String

alphaStar :: [String]
alphaStar = map (:[]) alpha ++ [ a : as | as <- alphaStar, a <- alpha ]
            where alpha = ['a'..'z']


freshPrim_ :: MonadState Int m => m String
freshPrim_ =
  do n <- get
     modify (+1)
     return (alphaStar !! n)

                  
instance MonadFresh FreshM where
  fresh =
    FreshM freshPrim_

instance Monad m => MonadFresh (FreshT m) where
  fresh =
    FreshT freshPrim_

testFresh :: FreshM String
testFresh = do fresh; fresh

runFresh :: FreshM b -> b
runFresh (FreshM m) = fst $ runState m 0

runFreshT :: Monad m => FreshT m b -> m b
runFreshT fm = fst <$> runStateT (unFreshT fm) 0
