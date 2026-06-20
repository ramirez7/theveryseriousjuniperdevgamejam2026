{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MJ626.Main where

import Lib
import Control.Monad (when)
import GHC.Wasm.Prim
import Pixi.Types qualified as Pixi
import Apecs
import MJ626.World
import MJ626.Text
import Data.Function (on)
import Safe (maximumMay, minimumByMay)
import Data.Foldable (for_)
import MJ626.Controls
import MJ626.Draw
import MJ626.Tick
import Linear.V2
import MJ626.Dir
import MJ626.Screen
import MJ626.Init
import MJ626.Env
import MJ626.Art
import MJ626.Jfxr.JSFFI qualified as Jfxr
import CuteC2

-- Export the actual initialization function
foreign export javascript "wasmMain" main :: IO ()


main :: IO ()
main = do
  envAudio <- Jfxr.newAudioContext
  consoleLogShow (c2Collide (c2Circle (C2V 0 0) 2) (c2Circle (C2V 3 3) 5))
  
  envArt <- newArt envAudio
  -- Initialize PIXI application
  envApp <- do
    x <- newApp
    x' <- initAppSized x gameWidth gameHeight
    appendCanvas x'
    resizeAppToScreen x'
    pure x'
  initGameFonts
  envPlayArea <- initPlayArea envApp
  envHammer <- getProperty "canvas" envApp >>= newDefaultHammer
  withEnv Env{..} $ do
    setScalingNearestNeighbor
    --appendToTarget "#canvas-container" app
    screen <- getProperty "screen" envApp
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    
    gameTicker <- newTicker
    setProperty "maxFPS" gameTicker (intAsVal 60)
    setProperty "minFPS" gameTicker (intAsVal 60)

    w <- initWorld
    runWith w $ do
      liftIO $ do
        -- foreground it
        addChild envApp envPlayArea
      newEntity_ Title >> screenTransition Title
    
    callAddTicker gameTicker =<< jsFuncFromHs_
      (\_ -> runWith w $ do
          tickFrame
          pure ()
          )
    
    handleInput w
    startTicker gameTicker
    pure ()
