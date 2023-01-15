{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module FailT.ExceptT where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Data.Functor.Identity
import Lib


newtype FailT m a = FailT (ExceptT String m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadFix, MonadTrans)

runFail :: FailT Identity a -> Either String a
runFail (FailT m) = runIdentity $ runExceptT m

runFailT :: FailT m a -> m (Either String a)
runFailT (FailT m) = runExceptT m

instance Monad m => MonadFail (FailT m) where
  fail = FailT . throwE
