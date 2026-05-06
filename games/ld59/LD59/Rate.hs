{-# LANGUAGE RecordWildCards #-}
module LD59.Rate where

import Data.Word
import LD59.World
import Control.Monad (when)
import Apecs

data Rate = Rate
  { ratePeriod :: Word64
  , rateOffset :: Word64
  }

snakeRate :: Rate
snakeRate = Rate 24 0

tailAnimRate :: Rate
tailAnimRate = Rate 10 0

scrambleAnimRate :: Rate
scrambleAnimRate = Rate 5 0

scrambleTickDegrees :: Int
scrambleTickDegrees = 30

spawnRate :: Rate
spawnRate = Rate (5 * 60) 25

everyFrameM :: System World Rate -> System World () -> System World ()
everyFrameM mr f = mr >>= flip everyFrame f

everyFrame :: Rate -> System World () -> System World ()
everyFrame Rate{..} k = do
  Frame frame <- get global
  when (frame `mod` ratePeriod == rateOffset) k

-- TODO: Unit test but it seems to work (see below)
rateTween :: Frame -> Rate -> Float
rateTween (Frame f) Rate{..} =
  if f < rateOffset
  then rateTween (Frame $ f + ratePeriod) Rate{..}
  else fromIntegral ((f - rateOffset) `mod` ratePeriod) / fromIntegral ratePeriod

{-
ghci> rateTween 5 (Rate 5 0)
0.0
ghci> rateTween 0 (Rate 5 0)
0.0
ghci> rateTween 4 (Rate 5 0)
0.8
ghci> rateTween 8 (Rate 5 0)
0.6
ghci> rateTween 8 (Rate 5 2)
0.2
ghci> rateTween 0 (Rate 5 1)
0.8
-}
