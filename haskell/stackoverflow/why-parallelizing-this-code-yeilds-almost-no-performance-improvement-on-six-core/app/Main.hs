module Main where

import Control.Parallel.Strategies
import Control.Monad
import Control.DeepSeq ( force , ($!!))
import Lib
import System.Environment (getArgs)
import Control.Concurrent.Async
--import UnliftIO.Async

main :: IO ()
main = do
    filename <- head <$> getArgs
    contents <- readFile filename
    case getSudokus contents of
        Just sudokus -> print $ map solve sudokus
        -- Just sudokus -> print $ runEval $ do
        --     start <- forM sudokus (rpar . force . solve)
        --     forM start rseq
        -- Just sudokus -> print =<< mapConcurrently (\s -> pure $!! solve s) sudokus
        Nothing -> putStrLn "Error during parsing"

