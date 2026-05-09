{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module LD59.Art where

import LD59.Wave
import Pixi.Types qualified as Pixi
import Lib
import LD59.Jfxr.Types
import LD59.Jfxr.JSFFI
import GHC.Wasm.Prim
import Data.Aeson qualified as Ae
import Data.String (fromString)

data Art = Art
  { artHeadTexture :: Pixi.Texture
  , artTailTexture :: Pixi.Texture
  , artBG :: Pixi.Texture
  , artBorderTop :: Pixi.Texture
  , artBorderSide :: Pixi.Texture
  , artHeadSide :: Pixi.Texture
  , artHeadUp :: Pixi.Texture
  , artSaw :: Pixi.Texture
  , artSine :: Pixi.Texture
  , artSquare :: Pixi.Texture
  , artTangent :: Pixi.Texture
  , artTriangle :: Pixi.Texture
  , artSinJfxr :: JfxrDef
  , artPlayingBGM :: AudioBuffer
  , artGameoverBGM :: AudioBuffer
  }

waveSpriteArt :: Art -> Wave -> Pixi.Texture
waveSpriteArt Art{..} = \case
  TRI -> artTriangle
  SIN -> artSine
  SQUARE -> artSquare
  SAW -> artSaw
  TAN -> artTangent

newArt :: AudioContext -> IO Art
newArt ac = do
  artHeadTexture <- loadTexture "./h.png"
  artTailTexture <- loadTexture "./t.png"
  artBG <- loadTexture "./BG.png"
  artBorderTop <- loadTexture "./Border Top.png"
  artBorderSide <- loadTexture "./Border side.png"
  artHeadSide <- loadTexture "./Head side.png"
  artHeadUp <- loadTexture "./Head up.png"
  artSaw <- loadTexture "./Saw.png"
  artSine <- loadTexture "./Sine.png"
  artSquare <- loadTexture "./Square.png"
  artTangent <- loadTexture "./Tangent.png"
  artTriangle <- loadTexture "./Triangle.png"
  artSinJfxr <- fetchJfxrDef "./ld59-sin.jfxr"
  artPlayingBGM <- fetchWav ac "./playing-ld59.wav"
  artGameoverBGM <- fetchWav ac "./gameover-ld59.wav"
  pure Art{..}

fetchJfxrDef :: JSString -> IO JfxrDef
fetchJfxrDef path = fetchText path >>= \js -> case Ae.eitherDecode (fromString (fromJSString js)) of
  Right jd -> pure jd
  Left e -> error e

