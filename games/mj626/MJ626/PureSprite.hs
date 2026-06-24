{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MJ626.PureSprite where

import Pixi.Types qualified as Pixi
import Lib
import Linear.V2
import Control.Monad.IO.Class
import GHC.Wasm.Prim

data PureSprite = PureSprite
  { psSprite :: Pixi.Sprite
  , psAnchor :: V2 Float
  , psScale :: V2 Float
  , psPosition :: V2 Float
  , psAngle :: Float
  }

mkPureSprite :: Pixi.Sprite -> PureSprite
mkPureSprite psSprite = PureSprite
  { psAnchor = V2 0 0
  , psScale = V2 1 1
  , psPosition = V2 0 0
  , psAngle = 0
  , psSprite
  }

withV2 :: V2 a -> (a -> a -> r) -> r
withV2 (V2 x y) k = k x y

setPropertyV2f :: IsJSVal a => JSString -> a -> V2 Float -> IO ()
setPropertyV2f k a v2f = withV2 v2f $ \x y -> do
  setPropertyKey [k, "x"] a (floatAsVal x)
  setPropertyKey [k, "y"] a (floatAsVal y)

syncPureSprite :: MonadIO m => PureSprite -> m ()
syncPureSprite PureSprite {..} = liftIO $ do
  setPropertyV2f "scale" psSprite psScale
  setPropertyV2f "position" psSprite psPosition
  setPropertyV2f "anchor" psSprite psAnchor
  setProperty "angle" psSprite (floatAsVal psAngle)
