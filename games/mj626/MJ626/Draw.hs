{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module MJ626.Draw where

import Lib
import Ease
import Control.Monad
import Pixi.Types qualified as Pixi
import MJ626.ECS
import MJ626.Rate
import Data.Foldable
import Apecs
import Control.Lens
import Linear.V2
import Linear.Vector ((^*), (*^))
import MJ626.Jfxr.Types
import MJ626.Jfxr.JSFFI
import GHC.Wasm.Prim
import Data.Aeson qualified as Ae
import Data.String (fromString)
import MJ626.Art
import MJ626.Env
import MJ626.Dir
import Control.Arrow (Kleisli (..))
import MJ626.Buffer
import Data.Maybe (fromMaybe)

setSpritePos :: Pixi.Sprite -> V2 Float -> IO ()
setSpritePos s (V2 x y) = do
  setProperty "x" s (floatAsVal x)
  setProperty "y" s (floatAsVal y)

setSpritePosOffset :: Pixi.Sprite -> V2 Int -> V2 Float -> IO ()
setSpritePosOffset s v2 offset = do
  let v2Screen = v2 ^* tileSize
  xAnchor <- valAsFloat <$> getPropertyKey ["anchor", "x"] s
  yAnchor <- valAsFloat <$> getPropertyKey ["anchor", "y"] s
  let xTileOff = round ((fromIntegral tileSize) * xAnchor)
  let yTileOff = round ((fromIntegral tileSize) * yAnchor)
  let tileOff = fmap fromIntegral (V2 xTileOff yTileOff)
  let scaledOffset = offset ^* (fromIntegral tileSize)
  let fullOff = tileOff + scaledOffset
  let (V2 screenX screenY) = (fmap fromIntegral v2Screen) + fullOff
  setProperty "x" s (floatAsVal screenX)
  setProperty "y" s (floatAsVal screenY)

unmirrorSpriteV :: Pixi.Sprite -> IO ()
unmirrorSpriteV s = setPropertyKey ["scale", "y"] s (intAsVal 1)

mirrorSpriteV :: Pixi.Sprite -> IO ()
mirrorSpriteV s = setPropertyKey ["scale", "y"] s (intAsVal (-1))

mirrorFlipSpriteV :: Pixi.Sprite -> IO ()
mirrorFlipSpriteV s = do
  y <- valAsInt <$> getPropertyKey ["scale", "y"] s
  setPropertyKey ["scale", "y"] s (intAsVal $ negate y)

unmirrorSpriteH :: Pixi.Sprite -> IO ()
unmirrorSpriteH s = setPropertyKey ["scale", "x"] s (intAsVal 1)

mirrorSpriteH :: Pixi.Sprite -> IO ()
mirrorSpriteH s = setPropertyKey ["scale", "x"] s (intAsVal (-1))

mirrorFlipSpriteH :: Pixi.Sprite -> IO ()
mirrorFlipSpriteH s = do
  y <- valAsInt <$> getPropertyKey ["scale", "x"] s
  setPropertyKey ["scale", "x"] s (intAsVal $ negate y)

centerAnchorSprite :: Pixi.Sprite -> IO ()
centerAnchorSprite s = do
  setPropertyKey ["anchor", "x"] s (floatAsVal 0.5)
  setPropertyKey ["anchor", "y"] s (floatAsVal 0.5)

setSpriteTexture :: Pixi.Sprite -> Pixi.Texture -> IO ()
setSpriteTexture s t = setProperty "texture" s t

rotateSprite :: Pixi.Sprite -> Float -> IO ()
rotateSprite s a = setProperty "rotation" s (floatAsVal a)

pieceEase :: Ord a => Fractional a => a -> Ease a -> Ease a -> Ease a
pieceEase cutoff ef1 ef2 = \x -> if x < cutoff then ef1 (x / cutoff) else ef2 ((x - cutoff) / (1 - cutoff))

invEase :: Num a => Ease a -> Ease a
invEase ef = \x -> ef (1 - x)
