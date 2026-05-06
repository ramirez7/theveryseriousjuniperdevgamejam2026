{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NegativeLiterals #-}
module LD59.Dir where

import Linear.V2

data Dir = UP | DOWN | LEFT | RIGHT
  deriving stock (Show, Eq)

oppositeDir :: Dir -> Dir
oppositeDir = \case
  UP -> DOWN
  DOWN -> UP
  LEFT -> RIGHT
  RIGHT -> LEFT

dirV2 :: Dir -> V2 Int
dirV2 = \case
  UP -> V2 0 -1
  DOWN -> V2 0 1
  LEFT -> V2 -1 0
  RIGHT -> V2 1 0

dirV2f :: Dir -> V2 Float
dirV2f = fmap fromIntegral . dirV2
