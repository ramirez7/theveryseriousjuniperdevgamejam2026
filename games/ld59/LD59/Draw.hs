{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module LD59.Draw where

import Lib
import Ease
import Control.Monad
import Pixi.Types qualified as Pixi
import LD59.World
import LD59.Snake
import LD59.Level
import LD59.Rate
import Data.Foldable
import Apecs
import Control.Lens
import Linear.V2
import Linear.Vector ((^*), (*^))
import LD59.Wave
import LD59.Jfxr.Types
import LD59.Jfxr.JSFFI
import GHC.Wasm.Prim
import Data.Aeson qualified as Ae
import Data.String (fromString)
import LD59.Art
import LD59.Env
import LD59.Dir
import Control.Arrow (Kleisli (..))
import LD59.Buffer
import Data.Maybe (fromMaybe)

test :: System World ()
test = cmapM_ $ \CurrentDir{} -> pure ()

setSpritePos :: Pixi.Sprite -> V2 Int -> IO ()
setSpritePos s p = setSpritePosOffset s p (V2 0 0)

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
-- TODO: Tune this (or throw it out)
snakeMoveEase :: Ease Float
snakeMoveEase = (* 0.75) . subtract 0.25
--snakeMoveEase = const 0 -- easy to remove it
--snakeMoveEase = \x -> if x < 0.5 then 0 else  expoIn x

syncSnakeArt :: HasEnv => System World ()
syncSnakeArt = openEnv $ \Env{..} -> cmapM_ $ \(s@Snake{..} :: Snake) -> do
  for_ snakeHead $ \Head{..} -> do
    let SnakeHead {..} = snakeHead
    let (headTex, headMirror) = case snakeHeadDir of
          UP -> (artHeadUp envArt, traverse_ Kleisli [unmirrorSpriteV, unmirrorSpriteH])
          DOWN -> (artHeadUp envArt, traverse_ Kleisli [mirrorSpriteV, unmirrorSpriteH])
          LEFT -> (artHeadSide envArt, traverse_ Kleisli [unmirrorSpriteH, unmirrorSpriteV])
          RIGHT -> (artHeadSide envArt, traverse_ Kleisli [mirrorSpriteH, unmirrorSpriteV])
    off <- (dirV2f snakeHeadDir ^*) . snakeMoveEase <$> (currSnakeRate >>= rateTweenM)
    Frame f <- Apecs.get global
    liftIO $ runKleisli headMirror headSprite
    liftIO $ setSpriteTexture headSprite headTex
    liftIO $ setSpritePosOffset headSprite snakeHeadPos off
  let tailLocs = snakeLocateTail s
  let fullLocs = snakeHeadPos snakeHead : tailLocs
  for_ (zip3 (snakeLocateTail s) fullLocs (snakeTailSegs snakeTail)) $ \(tailPos, nextPos, SnakeTailSeg{..}) -> do
    let Tail{..} = snakeTailVal
    let moveV2 = fmap fromIntegral $ nextPos - tailPos
    liftIO $ rotateSprite tailSprite (unangle moveV2)
    moveOff <- (moveV2 ^*) . snakeMoveEase <$> (currSnakeRate >>= rateTweenM)
    liftIO $ setSpritePosOffset tailSprite tailPos moveOff
    
