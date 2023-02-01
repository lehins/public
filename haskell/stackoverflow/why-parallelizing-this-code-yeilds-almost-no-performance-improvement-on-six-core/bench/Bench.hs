{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Parallel.Strategies
import Criterion.Main
import Lib
import UnliftIO.Async

main :: IO ()
main = do
  let filename = "sudoku.txt"
      readSudoku = maybe (error "No parse") pure . getSudokus =<< readFile filename
  sudokus <- force <$> readSudoku
  defaultMain
    [ bgroup
        "Sudoku"
        [ bgroup
            "Individual"
            [ bench (show i) $ nf solve s | (i :: Int, s) <- zip [0 ..] sudokus
            ]
        , bgroup
            "All"
            [ bench "Seq" $ nf (map solve) sudokus
            , bench "Par" $ nf baselinePar sudokus
            , bench "pooledForConcurrently" $ nf baselinePar sudokus
            ]
        ]
    ]

baselinePar :: [Sudoku] -> [Maybe Sudoku]
baselinePar sudokus =
  runEval $ do
    start <- forM sudokus (rpar . force . solve)
    forM start rseq

baselineUnliftIO :: [Sudoku] -> IO [Maybe Sudoku]
baselineUnliftIO sudokus = pooledForConcurrentlyN (length sudokus) sudokus (\s -> pure $!! solve s)
