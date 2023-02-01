module Main where

import Criterion.Main
import Vector

main :: IO ()
main = do
  let n = 7
  defaultMain
    [ bgroup "Bench"
      [ bench "baseline" $ nf example n
      , bench "Massiv" $ nf example' n
      -- , bench "vector-strategies" $ nf exampleParVector n
      -- , bench "Vector Scheduler (traverseConcurrently_)" $ nf exampleScheduler n
      , bench "parallel" $ nf examplePar n
      -- , bench "async" $ nf exampleAsync n
      , bench "scheduler" $ nf exampleScheduler n
      -- , bench "Vector UnlitfIO" $ nf exampleUnliftIO n
      ]
    ]
