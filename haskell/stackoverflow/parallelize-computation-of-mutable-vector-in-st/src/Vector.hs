{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vector
  ( example
  , example'
  , examplePar
  , exampleParVector
  , exampleAsync
  , exampleScheduler
  , exampleUnliftIO
  ) where

import Data.Massiv.Array as A hiding (forM_, map, sum, mapM)
import Data.Massiv.Array.Unsafe as A
import Control.Monad.Primitive
import qualified Data.Vector as Vb
import qualified Data.Vector.Mutable as Vm
import qualified Data.Vector.Generic.Mutable as Vg
import qualified Data.Vector.Generic as Gg
import Control.Monad.ST as ST ( ST, runST )
import Data.Foldable(forM_)
import Data.Char(digitToInt)
import Data.Vector.Strategies
import Control.Concurrent.Async (forConcurrently_)
import System.IO.Unsafe
import Control.Scheduler (Comp(Par), runBatch_, withScheduler_)
import UnliftIO.Async (pooledForConcurrently_)
import Control.Parallel (par)

examplePar :: Int -> Vb.Vector Int
examplePar n = runST $ do
  m <- Vg.new (2^n) :: ST s (Vm.STVector s Int)

  Vg.unsafeWrite m 0 (1)

  forM_ [1..n] $ \i -> do
    p <- prev m n (i-1)
    let newEntries = choiceList n i :: [Int]
    forM_ newEntries $ \e -> do
      let v = bogus p e
      v `par` Vg.unsafeWrite m e v

  Gg.unsafeFreeze m


exampleParVector :: Int -> Vb.Vector Int
exampleParVector n = example n `using` parVector (2 ^ (n - 5))

-- exampleMonadPar :: Int -> Vb.Vector Int
-- exampleMonadPar n = runST $ do
--   m <- Vg.new (2^n) :: ST s (Vm.STVector s Int)

--   Vg.unsafeWrite m 0 (1)

--   forM_ [1..n] $ \i -> do
--     p <- prev m n (i-1)
--     let newEntries = choiceList n i :: [Int]
--     forM_ newEntries $ \e -> do
--       let v = bogus p e
--       v `par` Vg.unsafeWrite m e v

--   Gg.unsafeFreeze m


exampleScheduler :: Int -> Vb.Vector Int
exampleScheduler n = unsafePerformIO $ do
  m <- Vg.new (2^n)

  Vg.unsafeWrite m 0 (1)

  withScheduler_ Par $ \scheduler ->
    forM_ [1..n] $ \i -> runBatch_ scheduler $ \_ -> do
      p <- prev m n (i-1)
      let newEntries = choiceList n i :: [Int]
      forM_ newEntries $ \e -> scheduleWork_ scheduler $ do
        let !v = bogus p e
        Vg.unsafeWrite m e v

  Gg.unsafeFreeze m


exampleUnliftIO :: Int -> Vb.Vector Int
exampleUnliftIO n = unsafePerformIO $ do
  m <- Vg.new (2^n)

  Vg.unsafeWrite m 0 (1)

  forM_ [1..n] $ \i -> do
    p <- prev m n (i-1)
    let newEntries = choiceList n i :: [Int]
    pooledForConcurrently_ newEntries $ \e -> do
      let !v = bogus p e
      Vg.unsafeWrite m e v

  Gg.unsafeFreeze m


exampleAsync :: Int -> Vb.Vector Int
exampleAsync n = unsafePerformIO $ do
  m <- Vg.new (2^n)

  Vg.unsafeWrite m 0 (1)

  forM_ [1..n] $ \i -> do
    p <- prev m n (i-1)
    let newEntries = choiceList n i :: [Int]
    forConcurrently_ newEntries $ \e -> do
      let !v = bogus p e
      Vg.unsafeWrite m e v

  Gg.unsafeFreeze m


example :: Int -> Vb.Vector Int
example n = runST $ do
  m <- Vg.new (2^n) :: ST s (Vm.STVector s Int)

  Vg.unsafeWrite m 0 (1)

  forM_ [1..n] $ \i -> do
    p <- prev m n (i-1)
    let newEntries = choiceList n i :: [Int]
    forM_ newEntries $ \e -> do
      let v = bogus p e
      Vg.unsafeWrite m e v

  Gg.unsafeFreeze m

example' :: Int -> Vb.Vector Int
example' n = runST $ do
  m <- Vg.new (2^n) :: ST s (Vm.STVector s Int)

  Vg.unsafeWrite m 0 (1)

  getEntries <- makeChoiceLists n

  forM_ [1..n] $ \i -> do
    p <- prev' getEntries m n (i-1)
    let newEntries = getEntries n i
    forM_ newEntries $ \e -> do
      let v = bogus p e
      v `par` Vg.unsafeWrite m e v

  Gg.unsafeFreeze m

makeChoiceLists :: forall m .(PrimMonad m, MonadThrow m) => Int -> m (Int -> Int -> [Int])
makeChoiceLists n = do
  ma :: MMatrix (PrimState m) BL [Int] <- A.unsafeNew (Sz2 (n + 1) (n + 1))
  forM_ [0 .. n] $ \i -> do
    A.unsafeWrite ma (i :. i) [2 ^ i - 1]
    A.unsafeWrite ma (i :. 0) [0]
    A.unsafeWrite ma (i :. 1) [2 ^ k | k <- [0 .. (i - 1)]]
  forM_ [2 .. n] $ \i -> do
    forM_ [2 .. i-1] $ \j -> do
      left <- A.unsafeRead ma (i - 1 :. j)
      right <- A.unsafeRead ma (i - 1 :. j - 1)
      let result = left ++ map ((2 ^ (i - 1)) +) right
      A.write ma (i :. j) result
  a <- A.unsafeFreeze Seq ma
  pure $ \ i j -> A.index' a (i :. j)
{-# INLINEABLE makeChoiceLists #-}

choiceList :: Int -> Int -> [Int]
choiceList _ 0 = [0]
choiceList n 1 = [ 2^k | k <- [0..(n-1) ] ]
choiceList n k
  | n == k = [2^n - 1]
  | otherwise = (choiceList (n-1) k) ++ (map ((2^(n-1)) +) $ choiceList (n-1) (k-1))

prev :: PrimMonad m => Vm.MVector (PrimState m) Int -> Int -> Int -> m Integer
prev _ _ 0 = return 1
prev m n i = do
  let chs = choiceList n i
  v <- mapM (\k -> Vg.unsafeRead m k ) chs
  let e = map (\k -> toInteger k ) v
  return (sum e)

prev' ::
  PrimMonad m => (Int -> Int -> [Int]) -> Vm.MVector (PrimState m) Int -> Int -> Int -> m Integer
prev' _ _ _ 0 = return 1
prev' f m n i = do
  let chs = f n i
  v <- mapM (\k -> Vg.unsafeRead m k ) chs
  let e = map (\k -> toInteger k ) v
  return (sum e)

bogus :: Integer -> Int -> Int
bogus prior index = do
  let f = fac prior
  let g = (f^index) :: Integer
  let d = (map digitToInt (show g)) :: [Int]
  let a = fromIntegral (head d) ^ (2 :: Int)
  a

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)
