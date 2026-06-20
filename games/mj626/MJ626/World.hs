{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module MJ626.World where

import Apecs
import Data.Word (Word64)
import Pixi.Types qualified as Pixi
import MJ626.Dir
import Data.Monoid (Sum (..), First (..))
import Linear.V2
import MJ626.Buffer
import MJ626.Jfxr.JSFFI (AudioBufferSourceNode)

data Screen = Title deriving stock (Show, Eq)

instance Component Screen where type Storage Screen = Unique Screen

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
makeWorld "World"
  [ ''Frame
  , ''Screen
  , ''BGM
  , ''UIText
  ]
