{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module LD59.Wave where

import Lib
import Pixi.Types qualified as Pixi

data Wave = TRI | SIN | SQUARE | SAW | TAN
  deriving (Show, Eq, Ord, Enum, Bounded)

waveSpriteTint :: Wave -> Pixi.Sprite -> IO ()
waveSpriteTint w s = setProperty "tint" s (stringAsVal tint)
  where
    tint = case w of
      TRI -> "yellow"
      SIN -> "blue"
      SQUARE -> "green"
      SAW -> "red"
      TAN -> "orange"
