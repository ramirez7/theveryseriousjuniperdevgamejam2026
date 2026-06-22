{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MJ626.Main where

import Lib
import Control.Monad (when)
import GHC.Wasm.Prim
import Pixi.Types qualified as Pixi
import Apecs
import MJ626.ECS
import MJ626.Text
import Data.Function (on)
import Safe (maximumMay, minimumByMay)
import Data.Foldable (for_)
import MJ626.Controls
import MJ626.Draw
import MJ626.Tick
import Linear.V2
import MJ626.Dir
import MJ626.Scene
import MJ626.Camera
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
  envCamera <- initCamera envApp
  envHammer <- getProperty "canvas" envApp >>= newDefaultHammer
  envECS <- initECS
  withEnv Env{..} $ do
    setScalingNearestNeighbor
    --appendToTarget "#canvas-container" app
    screen <- getProperty "screen" envApp
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    
    gameTicker <- newTicker
    setProperty "maxFPS" gameTicker (intAsVal 60)
    setProperty "minFPS" gameTicker (intAsVal 60)


    runWith envECS $ do
      liftIO $ do
        -- foreground it
        addChild envApp envCamera
      newEntity_ Title >> sceneTransition Title
      newEntity_ Camera
        { cameraFocus = V2 0 0
        , cameraHeight = fromIntegral gameHeight
        , cameraWidth = fromIntegral gameWidth
        }
      liftIO $ do
        bg <- baseTexture "WHITE" >>= newSprite
        setProperty "width" bg (intAsVal gameWidth)
        setProperty "height" bg (intAsVal gameHeight)
        setProperty "x" bg (intAsVal 0)
        setProperty "y" bg (intAsVal 0)
        addCameraChild bg
      testSprite <- liftIO $ newSprite (artTornadoTexture envArt)
      liftIO $ addCameraChild testSprite
      liftIO $ setSpritePos testSprite (V2 100 100)

    callAddTicker gameTicker =<< jsFuncFromHs_
      (\_ -> runWith envECS $ do
          tickFrame
          applyCamera
          pure ()
          )

    handleInput envECS
    startTicker gameTicker
    pure ()
