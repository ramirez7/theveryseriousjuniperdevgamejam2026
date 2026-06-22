{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module MJ626.Camera where

import MJ626.Env
import MJ626.ECS
import Pixi.Types qualified as Pixi
import Linear.V2
import Lib
import Apecs
import Control.Monad.IO.Class

applyCamera
  :: forall w m
   . Has w m Camera
  => HasEnv
  => MonadIO m
  => SystemT w m ()
applyCamera = openEnv $ \Env{..} -> cmapM_ $ \Camera{..} -> liftIO $ do
  setPropertyKey ["position", "x"] envCamera (floatAsVal (cameraWidth/2))
  setPropertyKey ["position", "y"] envCamera (floatAsVal (cameraHeight/2))

  let (V2 fx fy) = cameraFocus
  setPropertyKey ["pivot", "x"] envCamera (floatAsVal fx)
  setPropertyKey ["pivot", "y"] envCamera (floatAsVal fy)
