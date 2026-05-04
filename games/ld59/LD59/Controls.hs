{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module LD59.Controls where

import LD59.World
import LD59.Art
import GHC.Wasm.Prim
import LD59.Dir
import Apecs
import Control.Monad (unless, when)
import Lib
import Control.Monad.IO.Class
import LD59.Jfxr.Types
import LD59.Jfxr.JSFFI qualified as Jfxr
import Data.Coerce
import LD59.Screen
import Pixi.Types qualified as Pixi
import LD59.Draw
import LD59.Wave
import LD59.Init
import LD59.Env

jfxrStr :: JSString
jfxrStr = toJSString """
{"_version":1,"_name":"Default 1","_locked":[],"sampleRate":44100,"attack":0,"sustain":0.2,"sustainPunch":0,"decay":0,"tremoloDepth":0,"tremoloFrequency":10,"frequency":500,"frequencySweep":0,"frequencyDeltaSweep":0,"repeatFrequency":0,"frequencyJump1Onset":33,"frequencyJump1Amount":0,"frequencyJump2Onset":66,"frequencyJump2Amount":0,"harmonics":0,"harmonicsFalloff":0.5,"waveform":"sine","interpolateNoise":true,"vibratoDepth":0,"vibratoFrequency":10,"squareDuty":50,"squareDutySweep":0,"flangerOffset":0,"flangerOffsetSweep":0,"bitCrush":16,"bitCrushSweep":0,"lowPassCutoff":22050,"lowPassCutoffSweep":0,"highPassCutoff":0,"highPassCutoffSweep":0,"compression":1,"normalization":true,"amplification":100}
"""

handleInput :: HasEnv => World -> IO ()
handleInput w = openEnv $ \Env{..} -> do
  --ctx <- Jfxr.newAudioContext
  --clip <- Jfxr.newClip jfxrStr
  bindKeyDir w Playing "KeyS" DOWN
  bindKeyDir w Playing "KeyW" UP
  bindKeyDir w Playing "KeyA" LEFT
  bindKeyDir w Playing "KeyD" RIGHT
  bindKey w Dead "Enter" $ do
    cleanupSnake
    cleanupFood
    --liftIO $ Jfxr.newClip ((artSinJfxr envArt) { jfxrWaveform = waveToJfxr SAW }) >>= Jfxr.playClip envAudio
    initGame
    cmap $ \(_::Screen) -> Playing
{-  addWindowEventListener "keydown" =<< jsFuncFromHs_ (\_ -> do
                                                         consoleLogShow "PLAY"
                                                         consoleLogVal (coerce clip)
                                                         Jfxr.playClip ctx clip)-}
bindKey :: World -> Screen -> String -> System World () ->IO ()
bindKey w screen keycode sys =
  addWindowEventListener "keydown" =<< jsFuncFromHs_ (runWith w . gateKeypress keycode (gateScreen screen sys))

bindKeyDir :: World -> Screen -> String -> Dir -> IO ()
bindKeyDir w screen keycode dir = bindKey w screen keycode (setCurrentDir dir)

gateKeypress :: MonadIO m => String -> m () -> JSVal -> m ()
gateKeypress expectedCode k e = do
  krepeat <- valAsBool <$> liftIO (getProperty "repeat" e)
  unless krepeat $ do
    kcode <- fromJSString . valAsString <$> liftIO (getProperty "code" e)
    when (kcode == expectedCode) $ do
      k
  
setCurrentDir :: Dir -> System World ()
setCurrentDir dir = cmap $ \(CurrentDir _) -> CurrentDir dir
