{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module MJ626.Scene where

import Lib
import Control.Monad (when)
import Pixi.Types qualified as Pixi
import MJ626.ECS
import MJ626.Env
import MJ626.Text
import Apecs
import MJ626.BGM
import Apecs.Core

gateScene :: Scene -> System ECS () -> System ECS ()
gateScene scene sys = cmapM_ $ \theScene ->
  when (theScene == scene) sys

onText
  :: forall (tag :: *)
   . Has ECS IO tag
  => ExplGet IO (Storage tag)
  => ExplMembers IO (Storage tag)
  => (Pixi.Text -> System ECS ())
  -> System ECS ()
onText f = cmapM_ $ \(_::tag, UIText txt) ->
  f txt

sceneTransition :: HasEnv => Scene -> System ECS ()
sceneTransition next = cmapM $ \(curr::Scene) -> do
  sceneCleanup curr
  sceneInit next
  switchBGM next
  pure next

sceneCleanup :: HasEnv => Scene -> System ECS ()
sceneCleanup = \case
  Title -> pure ()

sceneInit :: HasEnv => Scene -> System ECS ()
sceneInit = \case
  Title -> pure ()
