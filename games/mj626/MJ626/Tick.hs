{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module MJ626.Tick where

import Apecs
import MJ626.ECS
import Ease
import MJ626.Art
import MJ626.Scene
import GHC.Wasm.Prim
import Control.Monad (when, guard)
import Control.Lens
import MJ626.Draw
import MJ626.Dir
import MJ626.BGM
import MJ626.Wave
import MJ626.Random
import Data.Foldable (traverse_)
import MJ626.Buffer
import Linear.V2
import Data.Foldable (for_)
import Lib
import Data.Set qualified as Set
import Pixi.Types qualified as Pixi
import MJ626.Env
import Data.Tuple.Extra (uncurry3)
import MJ626.Rate
import MJ626.Sfx

tickFrame :: System ECS ()
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
