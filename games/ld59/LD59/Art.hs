{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module LD59.Art where

import Pixi.Types qualified as Pixi
import Lib
import LD59.Jfxr.Types
import LD59.Jfxr.JSFFI
import GHC.Wasm.Prim
import Data.Aeson qualified as Ae
import Data.String (fromString)

data Art = Art
  { artHouseTexture :: Pixi.Texture
  , artTornadoTexture :: Pixi.Texture
  , artSinJfxr :: JfxrDef
  }

newArt :: AudioContext -> IO Art
newArt ac = do
  artHouseTexture <- loadTexture "./h.png"
  artTornadoTexture <- loadTexture "./t.png"
  artSinJfxr <- fetchJfxrDef "./ld59-sin.jfxr"
  pure Art{..}

fetchJfxrDef :: JSString -> IO JfxrDef
fetchJfxrDef path = fetchText path >>= \js -> case Ae.eitherDecode (fromString (fromJSString js)) of
  Right jd -> pure jd
  Left e -> error e

