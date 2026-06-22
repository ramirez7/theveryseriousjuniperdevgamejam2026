{-# LANGUAGE DerivingStrategies #-}

module MJ626.Coords where

import GHC.Wasm.Prim (JSVal)
import Lib (floatAsVal)

newtype Screen = Screen Float
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

screenAsVal :: Screen -> JSVal
screenAsVal (Screen f) = floatAsVal f

newtype World = World Float
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)
