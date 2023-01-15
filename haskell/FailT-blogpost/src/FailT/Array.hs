{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module FailT.Array where

import Control.Monad.ST (ST)
import Control.Monad.State (StateT, evalStateT, get, lift, put, replicateM_)
import Control.Monad.Trans.Fail.String (FailT, throwFailT)
import Data.Array (Array)
import Data.Array.ST (STArray, newArray_, runSTArray, writeArray)
import Lib (Hour, mkHour)

setHourArray :: STArray s Int Hour -> StateT Int (FailT (ST s)) ()
setHourArray arr = do
  ix <- get
  h <- mkHour $ toInteger ix
  lift $ lift $ writeArray arr ix h
  put (ix + 1)

hoursArray :: Int -> Array Int Hour
hoursArray n = runSTArray $ do
  mArr <- newArray_ (0, n - 1)
  throwFailT $ evalStateT (replicateM_ n (setHourArray mArr)) 0
  pure mArr
