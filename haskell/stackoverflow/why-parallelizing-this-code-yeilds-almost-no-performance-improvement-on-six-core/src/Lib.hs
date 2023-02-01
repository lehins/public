{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Lib (Sudoku (..), getSudokus, solve) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..))
import Control.Monad as M(replicateM)
import Control.Monad.ST
import Data.List (nub)
import Data.Vector as V (Vector, freeze, generate, thaw, (!), replicateM)
import qualified Data.Vector.Mutable as MV
import Text.Trifecta
import Data.Char (digitToInt)

data Cell
  = Given Int
  | Filled Int
  | Empty

instance NFData Cell where
  rnf = \case
    Given x -> rnf x
    Filled x -> rnf x
    Empty -> ()

newtype Sudoku = Sudoku (Vector Cell)
  deriving (NFData)

instance Show Cell where
  show Empty = "   "
  show (Filled x) = " " ++ show x ++ " "
  show (Given x) = "[" ++ show x ++ "]"

instance Show Sudoku where
  show (Sudoku vc) =
    unlines
      [ "+ -  -  - + -  -  - + -  -  - +"
      , "|" ++ i 0 ++ i 1 ++ i 2 ++ "|" ++ i 3 ++ i 4 ++ i 5 ++ "|" ++ i 6 ++ i 7 ++ i 8 ++ "|"
      , "|" ++ i 9 ++ i 10 ++ i 11 ++ "|" ++ i 12 ++ i 13 ++ i 14 ++ "|" ++ i 15 ++ i 16 ++ i 17 ++ "|"
      , "|" ++ i 18 ++ i 19 ++ i 20 ++ "|" ++ i 21 ++ i 22 ++ i 23 ++ "|" ++ i 24 ++ i 25 ++ i 26 ++ "|"
      , "+ -  -  - + -  -  - + -  -  - +"
      , "|" ++ i 27 ++ i 28 ++ i 29 ++ "|" ++ i 30 ++ i 31 ++ i 32 ++ "|" ++ i 33 ++ i 34 ++ i 35 ++ "|"
      , "|" ++ i 36 ++ i 37 ++ i 38 ++ "|" ++ i 39 ++ i 40 ++ i 41 ++ "|" ++ i 42 ++ i 43 ++ i 44 ++ "|"
      , "|" ++ i 45 ++ i 46 ++ i 47 ++ "|" ++ i 48 ++ i 49 ++ i 50 ++ "|" ++ i 51 ++ i 52 ++ i 53 ++ "|"
      , "+ -  -  - + -  -  - + -  -  - +"
      , "|" ++ i 54 ++ i 55 ++ i 56 ++ "|" ++ i 57 ++ i 58 ++ i 59 ++ "|" ++ i 60 ++ i 61 ++ i 62 ++ "|"
      , "|" ++ i 63 ++ i 64 ++ i 65 ++ "|" ++ i 66 ++ i 67 ++ i 68 ++ "|" ++ i 69 ++ i 70 ++ i 71 ++ "|"
      , "|" ++ i 72 ++ i 73 ++ i 74 ++ "|" ++ i 75 ++ i 76 ++ i 77 ++ "|" ++ i 78 ++ i 79 ++ i 80 ++ "|"
      , "+ -  -  - + -  -  - + -  -  - +"
      ]
    where
      i x = show (vc ! x)

parseSudoku :: Parser Sudoku
parseSudoku = do
  (Sudoku <$> V.replicateM 81 field) <* ((() <$ many newline) <|> eof)
  where
    field = (Empty <$ char '.') <|> (Given . digitToInt <$> digit)

-- parseSudoku :: Parser Sudoku
-- parseSudoku = do
--   lst <- M.replicateM 81 field
--   (newline *> return ()) <|> eof
--   return $ Sudoku $ generate 81 (lst !!)
--   where
--     field = (char '.' >> return Empty) <|> (Given . read . return <$> digit)

getSudokus :: String -> Maybe [Sudoku]
getSudokus raw = case parseString (some parseSudoku) mempty raw of
  Success ss -> Just ss
  Failure _ -> Nothing

data Direction = Back | Forward

solve :: Sudoku -> Maybe Sudoku
solve sudoku@(Sudoku puzzle) =
  if isValid sudoku
    then Just $ runST $ do
      puzzle' <- thaw puzzle
      go puzzle' 0 Forward
      Sudoku <$> freeze puzzle'
    else Nothing
  where
    go _ 81 _ = return ()
    go vector position direction = do
      cell <- MV.read vector position
      case (cell, direction) of
        (Empty, Back) -> error "Calling back Empty cell, this should not ever occur"
        (Empty, Forward) -> MV.write vector position (Filled 1) >> go vector position Forward
        (Given _, Back) -> go vector (position - 1) Back
        (Given _, Forward) -> go vector (position + 1) Forward
        (Filled 10, Back) -> MV.write vector position Empty >> go vector (position - 1) Back
        (Filled 10, Forward) -> go vector position Back
        (Filled x, Forward) -> do
          let (r, c, s) = calculatePositions position
          row <- getRowMV r vector
          col <- getColumnMV c vector
          sqr <- getSquareMV s vector
          if isUnique row && isUnique col && isUnique sqr
            then go vector (position + 1) Forward
            else MV.write vector position (Filled (x + 1)) >> go vector position Forward
        (Filled x, Back) -> MV.write vector position (Filled (x + 1)) >> go vector position Forward

calculatePositions :: Int -> (Int, Int, Int)
calculatePositions i =
  let (row, col) = divMod i 9
      sqr = (row `div` 3) * 3 + (col `div` 3)
   in (row, col, sqr)

isValid :: Sudoku -> Bool
isValid sudoku = go 0
  where
    go 9 = True
    go i = isUnique (getRow i sudoku) && isUnique (getColumn i sudoku) && isUnique (getSquare i sudoku) && go (i + 1)

getRow :: Int -> Sudoku -> [Cell]
getRow l (Sudoku vector) = go 0
  where
    go 9 = []
    go c = vector ! (l * 9 + c) : go (c + 1)

getRowMV :: MV.PrimMonad m => Int -> MV.MVector (MV.PrimState m) Cell -> m [Cell]
getRowMV l mv = go 0
  where
    go 9 = return []
    go c = do
      n <- MV.read mv (l * 9 + c)
      rl <- go (c + 1)
      return (n : rl)

getColumn :: Int -> Sudoku -> [Cell]
getColumn c (Sudoku vector) = go 0
  where
    go 9 = []
    go i = vector ! (c + i * 9) : go (i + 1)

getColumnMV :: MV.PrimMonad m => Int -> MV.MVector (MV.PrimState m) Cell -> m [Cell]
getColumnMV c mv = go 0
  where
    go 9 = return []
    go i = do
      n <- MV.read mv (c + i * 9)
      rl <- go (i + 1)
      return (n : rl)

getSquare :: Int -> Sudoku -> [Cell]
getSquare q (Sudoku vector) =
  let (y, x) = quotRem q 3
      start = x * 3 + y * 3 * 9
   in [ vector ! start
      , vector ! (start + 1)
      , vector ! (start + 2)
      , vector ! (start + 9)
      , vector ! (start + 10)
      , vector ! (start + 11)
      , vector ! (start + 18)
      , vector ! (start + 19)
      , vector ! (start + 20)
      ]

getSquareMV :: MV.PrimMonad m => Int -> MV.MVector (MV.PrimState m) a -> m [a]
getSquareMV q mv =
  let (y, x) = quotRem q 3
      start = x * 3 + y * 3 * 9
   in do
        a1 <- MV.read mv start
        a2 <- MV.read mv (start + 1)
        a3 <- MV.read mv (start + 2)
        b1 <- MV.read mv (start + 9)
        b2 <- MV.read mv (start + 10)
        b3 <- MV.read mv (start + 11)
        c1 <- MV.read mv (start + 18)
        c2 <- MV.read mv (start + 19)
        c3 <- MV.read mv (start + 20)
        return [a1, a2, a3, b1, b2, b3, c1, c2, c3]

isUnique :: [Cell] -> Bool
isUnique xs =
  let sv = strip xs
   in length sv == length (nub sv)
  where
    strip (Empty : xs) = strip xs
    strip ((Given x) : xs) = x : strip xs
    strip ((Filled x) : xs) = x : strip xs
    strip [] = []
