module LD59.Level where

import LD59.World
import LD59.Rate
import Apecs
import Data.Word

newtype Level = Level { unLevel :: Word64 } deriving Show

levelStep :: Word64
levelStep = 120

snakeLevel :: System World Level
snakeLevel = Apecs.get global >>= \(Score x) -> pure $ Level $ min 10 (succ $ x `div` levelStep)

snakeLevelRate :: Level -> Rate
snakeLevelRate (Level n) =
  snakeRate {ratePeriod = ratePeriod snakeRate - 2*n}

currSnakeRate :: System World Rate
currSnakeRate = snakeLevelRate <$> snakeLevel
