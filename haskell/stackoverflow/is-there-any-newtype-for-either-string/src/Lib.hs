-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Lib
  ( someFunc
  ) where

import Data.Time
import Data.Time.Format.ISO8601
import Control.Monad.Trans.Fail.String
import Control.Monad.Trans
import Control.Monad.Trans.Except

example :: Either String UTCTime
example =
  runFail $ iso8601ParseM "2023-01-16T22:14:00Z"

someFunc :: IO ()
someFunc = do
  pure ()
  -- read
  -- iso8601ParseM


data MyType a = Expected a | Unexpected a
  deriving (Show)

myFunc :: (Show a, MonadFail m) => m (MyType a) -> m String
myFunc t = do
  Expected a <- t
  pure $ show a



newtype ResultT m a = ResultT (ExceptT String m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadFail (ResultT m) where
  fail = ResultT . throwE

runResultT :: ResultT m a -> m (Either String a)
runResultT (ResultT m) = runExceptT m
