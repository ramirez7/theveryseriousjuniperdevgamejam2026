{-# LANGUAGE RecordWildCards #-}
module LD59.Tick where

import Apecs
import LD59.World
import LD59.Snake
import Data.Word
import Control.Monad (when)
import Control.Lens
import LD59.Draw
import LD59.Init
import Linear.V2
import Data.Foldable (for_)
import Lib
import Data.Set qualified as Set
import Pixi.Types qualified as Pixi
import Control.Monad.IO.Class
import LD59.Env
import LD59.Score

tickFrame :: System World ()
tickFrame = modify global (succ @Frame)

data Rate = Rate
  { ratePeriod :: Word64
  , rateOffset :: Word64
  }
snakeRate :: Rate
snakeRate = Rate 20 0

tailAnimRate :: Rate
tailAnimRate = Rate 10 0

spawnRate :: Rate
spawnRate = Rate (5 * 60) 27

worldBounds :: V2 Int
worldBounds = tileDims - pure 3 -- border + 1

worldCoords :: [V2 Int]
worldCoords = do
  let V2 wxb wyb = worldBounds
  x <- [0..wxb]
  y <- [0..wyb]
  [V2 x y]

everyFrame :: Rate -> System World () -> System World ()
everyFrame Rate{..} k = do
  Frame frame <- get global
  when (frame `mod` ratePeriod == rateOffset) k

cfoldMap
  :: forall c w a m
   . Apecs.Members w m c
  => Apecs.Get w m c
  => Monoid a
  => (c -> a)
  -> SystemT w m a
cfoldMap f = cfold (\acc c -> mappend acc (f c)) mempty

tickFoodSpawn :: HasEnv => System World ()
tickFoodSpawn = everyFrame spawnRate $ do
  snakeCoords <- cfoldMap $ \(s@Snake{..}::Snake) -> Set.fromList (snakeHeadPos snakeHead : snakeLocateTail s)
  foodCoords <- cfoldMap $ \Food{..} -> Set.singleton foodPos
  let occupiedCoords = mconcat [snakeCoords, foodCoords]
  let openCoords = filter (flip Set.notMember occupiedCoords) worldCoords
  wave <- randomFromList [minBound]
  randomFromList openCoords >>= newFood wave

animTail :: System World ()
animTail = everyFrame tailAnimRate $ do
  cmapM_ $ \(s::Snake) ->
    for_ (snakeTail s) $ \Tail{..} -> liftIO $ mirrorFlipSpriteH tailSprite

foodPoints :: Score
foodPoints = 10

tickSnake :: HasEnv => System World ()
tickSnake = everyFrame snakeRate $ do
  cmap $ \(CurrentDir dir, s::Snake) -> snakeMove dir s
  -- Check for match
  -- We do it _before_ eating, so for one tick the match
  -- is visible.
  cmapM $ \(s::Snake) -> do
    let (newSnake, match) = snakeMatch tailWave s
    for_ match $ \(t, _) -> do
      updateScore (+ (sum $ fmap (const foodPoints) t))
      cleanupSnakeTail t
    pure newSnake
  -- Check for eat
  cmapM_ $ \(Food{..}, foodEty) -> 
    cmapM_ $ \(s@Snake{..}::Snake, snakeEty) ->
      when (snakeHeadPos snakeHead == foodPos) $ do
        updateScore (+ foodPoints)
        Apecs.set snakeEty $ snakeEat id foodStuff s
        destroy foodEty (Proxy @Food)
  -- Check for death (tail or edge)
  cmapM_ $ \(s@Snake{..}::Snake) -> do
    let V2 hx hy = snakeHeadPos snakeHead
    let oob = hx < 0 || hy < 0 || hx > worldBounds ^. _x || hy > worldBounds ^. _y
    let onTail = snakeHeadPos snakeHead `elem` snakeLocateTail s
    when (oob || onTail) $ cmap $ \(_::Screen) -> Dead

randomFromList :: MonadIO m => [a] -> m a
randomFromList [] = error "randomFromList ERROR: empty list"
randomFromList xs = do
  n <- liftIO jsRandom
  pure $ xs !! floor (fromIntegral (length xs) * n)
