{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module FailT.Vector where

import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (StateT, evalStateT, get, lift, put, replicateM_)
import Control.Monad.Trans.Fail.String (FailT, runFailT, throwFailT)
import Data.Vector (Vector, freeze)
import Data.Vector.Mutable (MVector, new, write)
import Lib (Hour, mkHour)

setHourArray :: MVector s Hour -> FailT (StateT Int (ST s)) ()
setHourArray mVec = do
  ix <- get
  h <- mkHour $ toInteger ix
  lift $ write mVec ix h
  put (ix + 1)

hoursArrayEither :: Int -> Either String (Vector Hour)
hoursArrayEither n = runST $ do
  mVec <- new (max 0 n)
  res <- evalStateT (runFailT (replicateM_ n (setHourArray mVec))) 0
  mapM (const (freeze mVec)) res

hoursArray :: Int -> Vector Hour
hoursArray n = runST $ do
  mVec <- new (max 0 n)
  evalStateT (throwFailT (replicateM_ n (setHourArray mVec))) 0
  freeze mVec



