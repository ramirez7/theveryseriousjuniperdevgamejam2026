module LD59.Level where

import LD59.World
import LD59.Rate
import Apecs
import Data.Word

newtype Level = Level { unLevel :: Word64 } deriving Show

levelStep :: Word64
levelStep = 100

snakeLevel :: System World Level
snakeLevel = Apecs.get global >>= \(Score x) -> pure $ Level $ min 10 (succ $ x `div` levelStep)

snakeLevelRate :: Level -> Rate
snakeLevelRate (Level n) =
  --snakeRate {ratePeriod = ratePeriod snakeRate - n - (n `div` 2)}
  Rate (13 - n) 0

currSnakeRate :: System World Rate
currSnakeRate = snakeLevelRate <$> snakeLevel

