{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module LD59.Controls where

import LD59.World
import LD59.Buffer
import LD59.BGM
import LD59.Art
import Data.Foldable (traverse_)
import GHC.Wasm.Prim
import LD59.Dir
import Apecs
import Control.Monad (unless, when)
import Lib
import Control.Monad.IO.Class
import Data.Coerce
import LD59.Screen
import Pixi.Types qualified as Pixi
import LD59.Draw
import LD59.Wave
import LD59.Init
import LD59.Env
import LD59.Score
import LD59.Sfx
import Data.IORef
import Linear.V2
import Linear (norm)

jfxrStr :: JSString
jfxrStr = toJSString """
{"_version":1,"_name":"Default 1","_locked":[],"sampleRate":44100,"attack":0,"sustain":0.2,"sustainPunch":0,"decay":0,"tremoloDepth":0,"tremoloFrequency":10,"frequency":500,"frequencySweep":0,"frequencyDeltaSweep":0,"repeatFrequency":0,"frequencyJump1Onset":33,"frequencyJump1Amount":0,"frequencyJump2Onset":66,"frequencyJump2Amount":0,"harmonics":0,"harmonicsFalloff":0.5,"waveform":"sine","interpolateNoise":true,"vibratoDepth":0,"vibratoFrequency":10,"squareDuty":50,"squareDutySweep":0,"flangerOffset":0,"flangerOffsetSweep":0,"bitCrush":16,"bitCrushSweep":0,"lowPassCutoff":22050,"lowPassCutoffSweep":0,"highPassCutoff":0,"highPassCutoffSweep":0,"compression":1,"normalization":true,"amplification":100}
"""

handleInput :: HasEnv => World -> IO ()
handleInput w = openEnv $ \Env{..} -> do
  bindKeyDir w Playing ["KeyS", "ArrowDown"] DOWN
  bindKeyDir w Playing ["KeyW", "ArrowUp"] UP
  bindKeyDir w Playing ["KeyA", "ArrowLeft"] LEFT
  bindKeyDir w Playing ["KeyD", "ArrowRight"] RIGHT

  bindTouchControls w $ \case
    DoubleTap -> Apecs.get global >>= \case
      Title -> screenTransition Tutorial
      Tutorial ->screenTransition Playing
      Dead -> screenTransition Playing
      Playing -> do
        cmapM $ \(s::Snake, Not :: Not Scrambling) -> do
          playJfxr scrambleNoise
          pure $ Scrambling 3
    Swipe sv -> traverse_ setCurrentDir (v2Dir 45 sv)

  bindKey w Playing ["Space"] $ do
    cmapM $ \(s::Snake, Not :: Not Scrambling) -> do
      playJfxr scrambleNoise
      pure $ Scrambling 3
  bindKey w Title ["Enter"] $ do
    screenTransition Tutorial
    -- HACK
    liftIO $ bindKey w Tutorial ["Enter"] $ screenTransition Playing
  
  bindKey w Dead ["Enter"] $ screenTransition Playing

bindKey :: World -> Screen -> [String] -> System World () -> IO ()
bindKey w screen keycodes sys =
  addWindowEventListener "keydown" =<< jsFuncFromHs_ (runWith w . gateKeypress keycodes (gateScreen screen sys))

bindKeyDir :: World -> Screen -> [String] -> Dir -> IO ()
bindKeyDir w screen keycodes dir = bindKey w screen keycodes (setCurrentDir dir)

gateKeypress :: MonadIO m => [String] -> m () -> JSVal -> m ()
gateKeypress expectedCodes k e = do
  krepeat <- valAsBool <$> liftIO (getProperty "repeat" e)
  unless krepeat $ do
    kcode <- fromJSString . valAsString <$> liftIO (getProperty "code" e)
    when (kcode `elem` expectedCodes) $ do
      k

setCurrentDir :: Dir -> System World ()
setCurrentDir dir = cmap $ \(CurrentDir b) -> 
  CurrentDir (buffer dir b)

data TouchInput =
    DoubleTap
  | Swipe (V2 Float)
  deriving (Show)

bindTouchControls :: World -> (TouchInput -> System World ()) -> IO ()
bindTouchControls w f = do
  let swipeThreshold = 20 -- px
  let tapThreshold = 300 -- ms

  startRef <- liftIO $ newIORef (V2 0 0)
  lastTapRef <- liftIO $ newIORef 0

  addDocumentEventListenerHs "touchstart" $ \e -> do
    x <- liftIO $ pure . valAsFloat =<< getProperty "clientX" =<< getEventTouch e
    y <- liftIO $ pure . valAsFloat =<< getProperty "clientY" =<< getEventTouch e
    liftIO $ atomicWriteIORef startRef (V2 x y)

  addDocumentEventListenerHs "touchend" $ \e -> do
    endX <- liftIO $ pure . valAsFloat =<< getProperty "clientX" =<< getEventChangedTouch e
    endY <- liftIO $ pure . valAsFloat =<< getProperty "clientY" =<< getEventChangedTouch e
    startXY <- liftIO $ readIORef startRef

    t <- liftIO $ valAsFloat <$> jsGetTime
    lastTap <- readIORef lastTapRef
    let tapLen = t - lastTap
    liftIO $ writeIORef lastTapRef t
    
    let diffXY = V2 endX endY - startXY
    if | norm diffXY > swipeThreshold -> 
         runWith w (f $ Swipe diffXY)
       | tapLen < tapThreshold ->
         runWith w (f DoubleTap)
       | otherwise -> pure()
  
bindTouchSwipe :: World -> (V2 Float -> System World ()) -> IO ()
bindTouchSwipe w f = do
  startRef <- liftIO $ newIORef (V2 0 0)
  addDocumentEventListenerHs "touchstart" $ \e -> do
    x <- liftIO $ pure . valAsFloat =<< getProperty "clientX" =<< getEventTouch e
    y <- liftIO $ pure . valAsFloat =<< getProperty "clientY" =<< getEventTouch e
    liftIO $ atomicWriteIORef startRef (V2 x y)
  addDocumentEventListenerHs "touchend" $ \e -> do
    endX <- liftIO $ pure . valAsFloat =<< getProperty "clientX" =<< getEventChangedTouch e
    endY <- liftIO $ pure . valAsFloat =<< getProperty "clientY" =<< getEventChangedTouch e
    startXY <- liftIO $ readIORef startRef
    let diffXY = V2 endX endY - startXY
    runWith w (f diffXY)

bindTouchDoubleTap :: World -> System World () -> IO ()
bindTouchDoubleTap w k = do
  lastTapRef <- liftIO $ newIORef 0
  addDocumentEventListenerHs "touchend" $ \_ -> do
    t <- liftIO $ valAsFloat <$> jsGetTime
    lastTap <- readIORef lastTapRef
    let tapLen = t - lastTap
    liftIO $ writeIORef lastTapRef t
    when (tapLen < 300) $ runWith w k

bindTouchTwoFingerTap :: World -> System World () -> IO ()
bindTouchTwoFingerTap w k = do
  addDocumentEventListenerHs "touchend" $ \e -> do
    n <-getEventChangedTouchNum e
    when (n == 2) $ runWith w k
