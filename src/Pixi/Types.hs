module Pixi.Types where

import GHC.Wasm.Prim

newtype Application = Application JSVal
newtype Container = Container JSVal 
newtype Text = Text JSVal
newtype Sprite = Sprite JSVal
newtype Texture = Texture JSVal
newtype Ticker = Ticker JSVal
