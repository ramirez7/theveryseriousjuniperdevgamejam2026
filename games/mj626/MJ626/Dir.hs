{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NegativeLiterals #-}
module MJ626.Dir where

import Linear.V2
import Linear (signorm)
import Lib
import GHC.Wasm.Prim

data HDir = HLEFT | HRIGHT
  deriving stock (Show, Eq, Ord, Enum, Bounded)

flipHDir :: HDir -> HDir
flipHDir = \case
  HLEFT -> HRIGHT
  HRIGHT -> HLEFT

hdirV2 :: Num a => HDir -> V2 a
hdirV2 = \case
  HLEFT -> V2 -1 0
  HRIGHT -> V2 1 0

data Dir = UP | DOWN | LEFT | RIGHT
  deriving stock (Show, Eq, Ord, Enum, Bounded)


dirFromHammerEvent :: JSVal -> IO (Maybe Dir)
dirFromHammerEvent = fmap dirFromHammer . getProperty "direction"
  
dirFromHammer :: JSVal -> Maybe Dir
dirFromHammer j = case valAsInt j of
  2 -> Just LEFT
  4 -> Just RIGHT
  8 -> Just UP
  16 -> Just DOWN
  _ -> Nothing

dirIsTurn :: Dir -> Dir -> Bool
dirIsTurn curr next = next `notElem` [curr, oppositeDir curr]

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

-- | Convert degrees to radians
degToRad :: Float -> Float
degToRad d      = d * pi / 180
{-# INLINE degToRad #-}

-- | Convert radians to degrees
radToDeg :: Float -> Float
radToDeg r      = r * 180 / pi
{-# INLINE radToDeg #-}

-- 
v2Dir :: Float -> V2 Float -> [Dir]
v2Dir tolDeg v =
  let vdeg = radToDeg (unangle v)
      withinVdeg x = abs (vdeg - x) < tolDeg
  in filter (withinVdeg . radToDeg . unangle . dirV2f) [minBound..]
