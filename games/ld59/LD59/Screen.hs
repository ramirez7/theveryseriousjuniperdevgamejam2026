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
import LD59.Score
import LD59.Init
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
  Title -> do
    onText @TitleText textInvisible
    onText @PressStartText textInvisible
  Tutorial -> onText @TutorialText textInvisible
  Playing -> pure ()
  Dead -> do
    onText @GameOverText textInvisible
    onText @PressStartText textInvisible

screenInit :: HasEnv => Screen -> System World ()
screenInit = \case
  Title -> do
    onText @TitleText textVisible
    onText @PressStartText textVisible
  Tutorial -> onText @TutorialText $ \txt -> do
    textVisible txt
    -- This doesn't seem necessary, but is best practice due to
    -- HTMLText being rendered async
    liftIO $ onViewUpdate txt
  Playing -> do
    updateScore (const (Score 0))
    onText @ScoreText textVisible
    cleanupSnake
    cleanupFood
    initGame
  Dead -> do
    onText @GameOverText textVisible
    onText @PressStartText textVisible
    
