{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LD59.Init where

import LD59.World
import LD59.Dir
import LD59.Draw
import LD59.Text
import Pixi.Types qualified as Pixi
import Apecs
import Lib
import Linear.V2
import LD59.Env
import LD59.Art
import LD59.Random
import Apecs.Core

initPlayArea :: Pixi.Application -> IO Pixi.Container
initPlayArea app = do
  c <- newContainer
  addChild app c
  setProperty "x" c (intAsVal tileSize)
  setProperty "y" c (intAsVal tileSize)
  pure c

initText
  :: Has World IO tag
  => ExplSet IO (Storage tag)
  => HasEnv
  => tag
  -> IO Pixi.Text
  -> V2 Int
  -> System World ()
initText tt mkText (V2 tx ty) = openEnv $ \Env{..} -> do
  txt <- liftIO mkText
  liftIO $ do
    setProperty "x" txt (intAsVal tx)
    setProperty "y" txt (intAsVal ty)
    addChild envApp txt
    textInvisible txt
  newEntity_ (tt, UIText txt)
