{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad (when)
import GHC.Wasm.Prim
import Pixi.Types qualified as Pixi
import Apecs
import LD59.World
import Data.Function (on)
import Safe (maximumMay, minimumByMay)
import Data.Foldable (for_)
import LD59.Controls
import LD59.Draw
import LD59.Init
import LD59.Tick
import LD59.Snake
import Linear.V2
import LD59.Dir
import LD59.Screen
import LD59.Env
import LD59.Art
import LD59.Jfxr.JSFFI qualified as Jfxr

-- Export the actual initialization function
foreign export javascript "wasmMain" main :: IO ()


main :: IO ()
main = do
  ac <- Jfxr.newAudioContext

  art <- newArt ac
  -- Initialize PIXI application
  app <- do
    x <- newApp
    x' <- initAppSized x gameWidth gameHeight
    appendCanvas x'
    resizeAppToScreen x'
    pure x'
  pa <- initPlayArea app
  scoreTxt <- initScoreText app
  withEnv (Env art ac app pa scoreTxt) $ do
    setScalingNearestNeighbor
    --appendToTarget "#canvas-container" app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    
    gameTicker <- newTicker
    setProperty "maxFPS" gameTicker (intAsVal 60)
    setProperty "minFPS" gameTicker (intAsVal 60)
    
    
    
    w <- initWorld
    runWith w $ do
      initBG
      initBorder
      liftIO $ do
        -- foreground some stuff
        addChild app scoreTxt
        addChild app pa
      initGame
      syncSnakeArt
    
    callAddTicker gameTicker =<< jsFuncFromHs_
      (\_ -> runWith w $ do
          gateScreen Playing $ do
            tickFrame
            tickSnake
            tickFoodSpawn
            animTail
            animScramble
            syncSnakeArt
          )
    
    handleInput w
    startTicker gameTicker
    pure ()
