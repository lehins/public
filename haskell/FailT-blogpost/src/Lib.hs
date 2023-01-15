-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Trans.Fail.String
import Data.Aeson hiding (Array)
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.Binary hiding (get, put)
import qualified Data.Binary as B (get, put)
import Data.Int
import Data.Scientific
import Data.Time (Day)
import Data.Time.Format.ISO8601

newtype Hour = Hour Int8
  deriving (Show)

mkHour :: MonadFail m => Integer -> m Hour
mkHour h
  | h < 0 = fail $ "Hour cannot be negative, but got: " ++ show h
  | h > 23 = fail $ "Hour cannot be higher than 23, but got: " ++ show h
  | otherwise = pure $ Hour $ fromInteger h

instance FromJSON Hour where
  parseJSON = withScientific "Hour" $ \s -> do
    unless (isInteger s) $ fail "Expected an Integer"
    mkHour (truncate s)

instance Binary Hour where
  put (Hour h) = B.put h
  get = mkHour . (toInteger @Int8) =<< B.get

patternMatchFailure :: MonadFail m => Maybe Integer -> m Hour
patternMatchFailure mh = do
  Just h <- pure mh
  mkHour h

mkHourEither :: Integer -> Either String Hour
mkHourEither h
  | h < 0 = Left $ "Hour cannot be negative, but got: " ++ show h
  | h > 23 = Left $ "Hour cannot be higher than 23, but got: " ++ show h
  | otherwise = Right $ Hour $ fromInteger h

mkHour' :: MonadFail m => Integer -> m Hour
mkHour' = either fail pure . mkHourEither

data DayOfTheWeek
  = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Bounded, Enum)

dayOfTheWeekParsers :: (Alternative m, MonadFail m) => String -> [m DayOfTheWeek]
dayOfTheWeekParsers str =
  [ if show day == str then pure day else fail $ "Bad input " ++ str
  | day <- [minBound .. maxBound]
  ]

parseWeekend :: (Alternative f, MonadFail f) => String -> f DayOfTheWeek
parseWeekend str =
  (if str == "Sunday" then pure Sunday else fail "Not Sunday")
    <|> (if str == "Saturday" then pure Saturday else fail "Not Saturday")

-- | Return `Nothing` whenever a `Right` is supplied and `Just` value otherwise
leftToJust :: Either a b -> Maybe a
leftToJust e = do
  Left r <- Just e
  Just r
