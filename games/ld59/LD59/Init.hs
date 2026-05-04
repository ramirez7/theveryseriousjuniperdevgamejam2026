{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LD59.Init where

import LD59.World
import LD59.Snake
import LD59.Dir
import LD59.Draw
import Pixi.Types qualified as Pixi
import Apecs
import Lib
import Linear.V2
import Data.Foldable (for_)
import LD59.Wave
import Data.Traversable (for)
import LD59.Env
import LD59.Art


newFood :: HasEnv => Wave -> V2 Int -> System World ()
newFood tailWave p = openEnv $ \Env{..} -> do
  tailSprite <- liftIO $ newSprite (waveSpriteArt envArt tailWave)
  liftIO $ centerAnchorSprite tailSprite
  liftIO $ setSpritePos tailSprite p
  liftIO $ addPlayAreaChild tailSprite
  newEntity_ $ Food Tail{..} p

initGame :: HasEnv => System World ()
initGame = openEnv $ \Env{..} -> do
  headSprite <- liftIO $ newSprite (artHeadTexture envArt)
  liftIO $ do
    centerAnchorSprite headSprite
    addPlayAreaChild headSprite
  hardcodedTail <- for [minBound..] $ \tailWave -> do
    tailSprite <- liftIO $ newSprite (waveSpriteArt envArt tailWave)
    liftIO $ centerAnchorSprite tailSprite
    liftIO $ addPlayAreaChild tailSprite
    let snakeTailVal = Tail{..}
    let snakeTailDir = RIGHT
    pure SnakeTailSeg{..}
  let initSnake = Snake
        { snakeHead = SnakeHead
          { snakeHeadVal = Head { .. }
          , snakeHeadPos = V2 5 5
          , snakeHeadDir = RIGHT
          }
        , snakeTail = SnakeTail hardcodedTail
        , snakeStomachDir = RIGHT
        }
  newEntity_ (CurrentDir RIGHT, initSnake)
  newEntity_ Dead
  newFood TRI (V2 8 8)

initBG :: HasEnv => System World ()
initBG = openEnv $ \Env{..} -> do
  bgs <- liftIO $ newTilingSprite (artBG envArt) playAreaWidth playAreaHeight
  liftIO $ setSpritePos bgs (V2 0 0)
  liftIO $ addPlayAreaChild bgs
  Apecs.set global (BG $ Just bgs)

initBorder :: HasEnv => System World ()
initBorder = do
  Apecs.set global . Border =<< sequence
    [ mkBorderSprite artBorderTop (V2 0 0) gameWidth (tileSize)
    , mkBorderSprite artBorderTop (V2 0 (tileHeight-1)) gameWidth tileSize
    , mkBorderSprite artBorderSide (V2 0 1) tileSize (gameHeight - tileSize*2)
    , mkBorderSprite artBorderSide (V2 (tileWidth-1) 1) tileSize (gameHeight - tileSize*2)
    ]
  where
    mkBorderSprite f p w h = openEnv $ \Env{..} -> liftIO $ do
      b <- liftIO $ newTilingSprite (f envArt) w h
      setSpritePos b p
      liftIO $ addChild envApp b
      pure b

initPlayArea :: Pixi.Application -> IO Pixi.Container
initPlayArea app = do
  c <- newContainer
  addChild app c
  setProperty "x" c (intAsVal tileSize)
  setProperty "y" c (intAsVal tileSize)
  pure c

cleanupSnake :: System World ()
cleanupSnake = cmapM $ \(Snake{..}::Snake) -> do
  liftIO $ for_ snakeHead  $ \Head{..} -> destroySprite headSprite
  cleanupSnakeTail snakeTail
  pure (Nothing :: Maybe Snake)

cleanupSnakeTail :: SnakeTail Tail -> System World ()
cleanupSnakeTail st = liftIO $ for_ st $ \Tail{..} -> destroySprite tailSprite

cleanupFood :: System World ()
cleanupFood = cmapM $ \Food{..} -> do
  liftIO $ destroySprite (tailSprite foodStuff)
  pure (Nothing :: Maybe Food)
  
initScoreText :: Pixi.Application -> IO Pixi.Text
initScoreText app = do
  txt <- newText "0" "red"
  setProperty "x" txt (intAsVal $ gameWidth `div` 2)
  setProperty "y" txt (intAsVal $ 0)
  addChild app txt
  pure txt
