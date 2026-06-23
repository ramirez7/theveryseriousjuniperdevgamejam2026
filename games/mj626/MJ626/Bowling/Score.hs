module MJ626.Bowling.Score where

import MJ626.Bowling.Pin

type Roll = [Pin]

type Game = [Roll]

data Score = Score
  { scores :: [Frame]
  , nextPins :: [Pin]
  }

score :: Game -> Score
score = undefined

data Frame where
  OpenFrame :: [Pin] -> [Pin] -> Frame
  SpareFrame :: [Pin] -> Frame
  StrikeFrame :: Frame
  TenthFrame :: [Pin] -> [Pin] -> Frame

