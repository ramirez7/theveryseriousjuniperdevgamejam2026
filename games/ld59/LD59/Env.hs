{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module LD59.Env where

import LD59.Art
import LD59.Jfxr.JSFFI qualified as Jfxr
import Pixi.Types qualified as Pixi
import Control.Monad.IO.Class
import Lib
import Linear.V2
import Hammer.Types qualified as Hammer

data Env = Env
  { envArt :: Art
  , envAudio :: Jfxr.AudioContext
  , envApp :: Pixi.Application
  , envPlayArea :: Pixi.Container
  , envHammer :: Hammer.Manager
  }

type HasEnv = (?env :: Env)

withEnv :: Env -> (HasEnv => r) -> r
withEnv e k = let ?env = e in k

openEnv :: HasEnv => (Env -> r) -> r
openEnv k = k ?env


tileSize :: Int
tileSize = 32

tileDims :: V2 Int
tileWidth, tileHeight :: Int
tileDims@(V2 tileWidth tileHeight) = V2 12 20

gameDims :: V2 Int
gameWidth, gameHeight :: Int
gameDims@(V2 gameWidth gameHeight) = V2 (tileWidth*tileSize) (tileHeight*tileSize)

playAreaWidth, playAreaHeight :: Int
(V2 playAreaWidth playAreaHeight) = gameDims - pure (tileSize*2)

addPlayAreaChild :: MonadIO m => IsJSVal a => HasEnv => a -> m ()
addPlayAreaChild x = openEnv $ \Env{..} -> liftIO $ addContainerChild envPlayArea x

