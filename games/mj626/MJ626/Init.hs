{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module MJ626.Init where

import MJ626.ECS
import MJ626.Dir
import MJ626.Draw
import MJ626.Text
import Pixi.Types qualified as Pixi
import Apecs
import Lib
import Linear.V2
import MJ626.Env
import MJ626.Art
import MJ626.Random
import Apecs.Core

initCamera :: Pixi.Application -> IO Pixi.Container
initCamera app = do
  c <- newContainer
  addChild app c
  pure c

initText
  :: Has ECS IO tag
  => ExplSet IO (Storage tag)
  => HasEnv
  => tag
  -> IO Pixi.Text
  -> V2 Int
  -> System ECS ()
initText tt mkText (V2 tx ty) = openEnv $ \Env{..} -> do
  txt <- liftIO mkText
  liftIO $ do
    setProperty "x" txt (intAsVal tx)
    setProperty "y" txt (intAsVal ty)
    addChild envApp txt
    textInvisible txt
  newEntity_ (tt, UIText txt)
