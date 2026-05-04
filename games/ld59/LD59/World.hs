{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module LD59.World where

import Apecs
import Data.Word (Word64)
import Pixi.Types qualified as Pixi
import LD59.Snake
import LD59.Dir
import Data.Monoid (Sum (..), First (..))
import Linear.V2
import LD59.Wave
import LD59.Buffer

data Screen = Title | Playing | Dead deriving stock (Show, Eq)

instance Component Screen where type Storage Screen = Unique Screen

newtype BG = BG { bgSprite :: Maybe Pixi.Sprite }
  deriving (Semigroup, Monoid) via (First Pixi.Sprite)

instance Component BG where type Storage BG = Global BG

newtype Border = Border { borderSprites :: [Pixi.Sprite] }
  deriving (Semigroup, Monoid) via ([Pixi.Sprite])

instance Component Border where type Storage Border = Global Border

newtype CurrentDir = CurrentDir (Buffer 3 Dir) deriving stock (Show)
instance Component CurrentDir where type Storage CurrentDir = Unique CurrentDir

data Head = Head
  { headSprite :: Pixi.Sprite
  }

data Tail = Tail
  { tailSprite :: Pixi.Sprite
  , tailWave :: Wave
  }

type Snake = SnakeF Head Tail
data Food = Food
  { foodStuff :: Tail
  , foodPos :: V2 Int
  }

instance Component Food where type Storage Food = Map Food

newtype Frame = Frame Word64
  deriving stock (Show)
  deriving newtype (Enum, Bounded, Num)
  deriving (Semigroup, Monoid) via (Sum Frame)

instance Component Frame where type Storage Frame = Global Frame

newtype Score = Score { rawScore :: Word64 }
  deriving stock (Show)
  deriving newtype (Enum, Bounded, Num)
  deriving (Semigroup, Monoid) via (Sum Score)

instance Component Score where type Storage Score = Global Score


makeWorld "World" [''Snake, ''CurrentDir, ''Frame, ''Screen, ''Food, ''BG, ''Border, ''Score]
