{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Control.Monad.Trans.Fail.String
import Control.Monad.Trans.Except



newtype ResultT m a = ResultT (ExceptT String m a)
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadFail (ResultT m) where
  fail = ResultT . throwE

runResultT :: ResultT m a -> m (Either String a)
runResultT (ResultT m) = runExceptT m



data MyType a = Expected a | Unexpected a
  deriving (Show)

myFunc :: (Show a, MonadFail m) => m (MyType a) -> m String
myFunc t = do
  Expected a <- t
  pure $ show a
