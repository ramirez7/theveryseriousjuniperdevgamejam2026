{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LD59.Score where

import Lib
import LD59.World
import LD59.Env
import Control.Monad.IO.Class
import Apecs
import GHC.Wasm.Prim
import Pixi.Types qualified as Pixi

updateScore
  :: HasEnv
  => (Score -> Score)
  -> System World ()
updateScore f = openEnv $ \Env{..} -> do
  oldScore <- Apecs.get global
  let newScore = f oldScore
  Apecs.set global newScore
  liftIO $ setProperty "text" envScore (stringAsVal $ toJSString $ show $ rawScore newScore)
