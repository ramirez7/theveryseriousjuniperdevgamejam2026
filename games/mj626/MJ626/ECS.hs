{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module MJ626.ECS where

import Apecs
import Data.Word (Word64)
import Pixi.Types qualified as Pixi
import MJ626.Dir
import Data.Monoid (Sum (..), First (..))
import Linear.V2
import MJ626.Buffer
import MJ626.Jfxr.JSFFI (AudioBufferSourceNode)
import GHC.Generics

data Scene =
    Title
  | Bowling'Pre
  | Bowling'Rolling
  | Bowling'Pins
  deriving stock (Show, Eq)

instance Component Scene where type Storage Scene = Unique Scene

newtype Frame = Frame Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, Bounded, Num)
  deriving (Semigroup, Monoid) via (Sum Frame)

instance Component Frame where type Storage Frame = Global Frame

newtype BGM = BGM { bgmAudio :: Maybe AudioBufferSourceNode }
  deriving (Semigroup, Monoid) via (First AudioBufferSourceNode)
instance Component BGM where type Storage BGM = Global BGM

newtype UIText = UIText Pixi.Text
instance Component UIText where type Storage UIText = Map UIText

data Camera = Camera
  { cameraFocus :: V2 Float
  , cameraWidth :: Float
  , cameraHeight :: Float
  }
instance Component Camera where type Storage Camera = Unique Camera

data Tornado = Tornado
instance Component Tornado where type Storage Tornado = Unique Tornado

data TornadoGfxF a = TornadoGfx
  { tornadoSprite :: a
  }
  deriving stock (Show, Eq, Ord, Generic, Generic1, Functor, Foldable, Traversable)
  deriving Applicative via Generically1 TornadoGfxF

data TornadoDir = TornadoDir HDir
instance Component TornadoDir where type Storage TornadoDir = Unique TornadoDir

data Position = Position (V2 Float)
instance Component Position where type Storage Position = Map Position

data Velocity = Velocity (V2 Float)
instance Component Velocity where type Storage Velocity = Map Velocity

data Traction = Traction (V2 Float)
instance Component Traction where type Storage Traction = Map Traction

data Accel = Accel (V2 Float)
instance Component Accel where type Storage Accel = Map Accel

makeWorld "ECS"
  [ ''Frame
  , ''Scene
  , ''BGM
  , ''UIText
  , ''Camera
  , ''Tornado
  , ''TornadoDir
  , ''Position
  , ''Velocity
  ]
