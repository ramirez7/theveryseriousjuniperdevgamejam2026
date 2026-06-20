{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module LD59.Screen where

import Lib
import Control.Monad (when)
import Pixi.Types qualified as Pixi
import LD59.World
import LD59.Env
import LD59.Text
import Apecs
import LD59.BGM
import Apecs.Core

gateScreen :: Screen -> System World () -> System World ()
gateScreen screen sys = cmapM_ $ \theScreen ->
  when (theScreen == screen) sys

onText
  :: forall (tag :: *)
   . Has World IO tag
  => ExplGet IO (Storage tag)
  => ExplMembers IO (Storage tag)
  => (Pixi.Text -> System World ())
  -> System World ()
onText f = cmapM_ $ \(_::tag, UIText txt) ->
  f txt

screenTransition :: HasEnv => Screen -> System World ()
screenTransition next = cmapM $ \(curr::Screen) -> do
  screenCleanup curr
  screenInit next
  switchBGM next
  pure next

screenCleanup :: HasEnv => Screen -> System World ()
screenCleanup = \case
  Title -> pure ()

screenInit :: HasEnv => Screen -> System World ()
screenInit = \case
  Title -> pure ()
