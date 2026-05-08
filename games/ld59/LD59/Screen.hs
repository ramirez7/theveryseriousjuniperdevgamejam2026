{-# LANGUAGE LambdaCase #-}
module LD59.Screen where

import Control.Monad (when)
import LD59.World
import LD59.Env
import Apecs
import LD59.BGM
import LD59.Score
import LD59.Init

gateScreen :: Screen -> System World () -> System World ()
gateScreen screen sys = cmapM_ $ \theScreen ->
  when (theScreen == screen) sys

screenTransition :: HasEnv => Screen -> System World ()
screenTransition next = cmapM $ \(curr::Screen) -> do
  screenCleanup curr
  screenInit next
  switchBGM next
  pure next

screenCleanup :: HasEnv => Screen -> System World ()
screenCleanup = \case
  Title -> pure ()
  Playing -> pure ()
  Dead -> pure ()

screenInit :: HasEnv => Screen -> System World ()
screenInit = \case
  Title -> pure ()
  Playing -> do
    updateScore (const (Score 0))
    cleanupSnake
    cleanupFood
    initGame
  Dead -> pure ()
