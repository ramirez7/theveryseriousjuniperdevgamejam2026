{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module LD59.Tick where

import Apecs
import LD59.World
import Ease
import LD59.Art
import LD59.Screen
import GHC.Wasm.Prim
import Control.Monad (when, guard)
import Control.Lens
import LD59.Draw
import LD59.Dir
import LD59.BGM
import LD59.Wave
import LD59.Random
import Data.Foldable (traverse_)
import LD59.Buffer
import Linear.V2
import Data.Foldable (for_)
import Lib
import Data.Set qualified as Set
import Pixi.Types qualified as Pixi
import LD59.Env
import Data.Tuple.Extra (uncurry3)
import LD59.Rate
import LD59.Sfx

tickFrame :: System World ()
tickFrame = modify global (succ @Frame)

worldBounds :: V2 Int
worldBounds = tileDims - pure 3 -- border + 1

worldCoords :: [V2 Int]
worldCoords = do
  let V2 wxb wyb = worldBounds
  x <- [0..wxb]
  y <- [0..wyb]
  [V2 x y]

cfoldMap
  :: forall c w a m
   . Apecs.Members w m c
  => Apecs.Get w m c
  => Monoid a
  => (c -> a)
  -> SystemT w m a
cfoldMap f = cfold (\acc c -> mappend acc (f c)) mempty
