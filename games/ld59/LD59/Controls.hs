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
import Data.List (sort)
import Data.Foldable (for_)
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
import Hammer.Types qualified as Hammer
import LD59.Draw
import LD59.Wave
import LD59.Env
import Data.IORef
import Linear.V2
import Linear (norm)

handleInput :: HasEnv => World -> IO ()
handleInput w = openEnv $ \Env{..} -> do
  pure ()

bindKey :: World -> Screen -> [String] -> System World () -> IO ()
bindKey w screen keycodes sys =
  addWindowEventListener "keydown" =<< jsFuncFromHs_ (runWith w . gateKeypress keycodes (gateScreen screen sys))

gateKeypress :: MonadIO m => [String] -> m () -> JSVal -> m ()
gateKeypress expectedCodes k e = do
  krepeat <- valAsBool <$> liftIO (getProperty "repeat" e)
  unless krepeat $ do
    kcode <- fromJSString . valAsString <$> liftIO (getProperty "code" e)
    when (kcode `elem` expectedCodes) $ do
      k
