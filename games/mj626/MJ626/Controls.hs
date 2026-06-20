{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module MJ626.Controls where

import MJ626.World
import MJ626.Buffer
import MJ626.BGM
import Data.List (sort)
import Data.Foldable (for_)
import MJ626.Art
import Data.Foldable (traverse_)
import GHC.Wasm.Prim
import MJ626.Dir
import Apecs
import Control.Monad (unless, when)
import Lib
import Control.Monad.IO.Class
import Data.Coerce
import MJ626.Screen
import Pixi.Types qualified as Pixi
import Hammer.Types qualified as Hammer
import MJ626.Draw
import MJ626.Wave
import MJ626.Env
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
