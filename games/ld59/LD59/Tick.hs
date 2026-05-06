{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module LD59.Tick where

import Apecs
import LD59.World
import LD59.Snake
import Data.Word
import Control.Monad (when)
import Control.Lens
import LD59.Draw
import LD59.Init
import LD59.Wave
import LD59.Random
import Data.Foldable (traverse_)
import LD59.Buffer
import Data.Maybe (fromMaybe)
import Linear.V2
import Data.Foldable (for_)
import Lib
import Data.Set qualified as Set
import Pixi.Types qualified as Pixi
import LD59.Env
import LD59.Score
import Data.Tuple.Extra (uncurry3)
import LD59.Rate

tickFrame :: System World ()
tickFrame = modify global (succ @Frame)

worldBounds :: V2 Int
worldBounds = tileDims - pure 3 -- border + 1

worldCoords :: [V2 Int]
worldCoords = do
  let V2 wxb wyb = worldBounds
  x <- [0..wxb]
  y <- [0..wyb]
  [V2 x y]

cfoldMap
  :: forall c w a m
   . Apecs.Members w m c
  => Apecs.Get w m c
  => Monoid a
  => (c -> a)
  -> SystemT w m a
cfoldMap f = cfold (\acc c -> mappend acc (f c)) mempty

listOpenCoords :: System World [V2 Int]
listOpenCoords = do
  snakeCoords <- cfoldMap $ \(s@Snake{..}::Snake) -> Set.fromList (snakeHeadPos snakeHead : snakeLocateTail s)
  foodCoords <- cfoldMap $ \Food{..} -> Set.singleton foodPos
  let occupiedCoords = mconcat [snakeCoords, foodCoords]
  pure $ filter (flip Set.notMember occupiedCoords) worldCoords  

newRandomFood :: HasEnv => [Wave] ->V2 Int -> System World ()
newRandomFood waves p = do
  wave <- randomFromList waves
  newFood wave p

tickFoodSpawn :: HasEnv => System World ()
tickFoodSpawn = everyFrame spawnRate $
  listOpenCoords >>= randomFromList >>= newRandomFood [minBound..maxBound]

animTail :: System World ()
animTail = everyFrame tailAnimRate $ do
  cmapM_ $ \(s::Snake) ->
    for_ (snakeTail s) $ \Tail{..} -> liftIO $ mirrorFlipSpriteH tailSprite

animScramble :: System World ()
animScramble = everyFrame scrambleAnimRate $ do
  cmapM_ $ uncurry $ \(s::Snake) -> \case
    Nothing -> for_ (snakeHead s) $ \Head{..} -> liftIO $ setProperty "angle" headSprite (intAsVal 0)
    Just (Scrambling{}) -> for_ (snakeHead s) $ \Head{..} -> liftIO $ do
      d <- valAsInt <$> getProperty "angle" headSprite
      setProperty "angle" headSprite (intAsVal $ d + scrambleTickDegrees)

foodPoints :: Score
foodPoints = 10

tickSnake :: HasEnv => System World ()
tickSnake = everyFrame snakeRate $ do
  cmapM $ \(CurrentDir b, s::Snake) -> do
    currFrame <- Apecs.get global
    let halfPeriod = Frame (ratePeriod snakeRate `div` 2)
    let frameCutoff = if currFrame < halfPeriod then 0 else currFrame - halfPeriod
    let (mDir, b') = unbufferWhen ((< frameCutoff) . inputFrame) b
        dir = maybe (snakeHeadDir $ snakeHead s) inputDir mDir
    pure (snakeMove dir s, CurrentDir b')
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
    cmapM_ $ uncurry3 $ \(s@Snake{..}::Snake) -> \snakeEty -> \case
      Nothing -> do
        when (snakeHeadPos snakeHead == foodPos) $ do
          updateScore (+ foodPoints)
          Apecs.set snakeEty $ snakeEat id foodStuff s
          destroy foodEty (Proxy @Food)
      Just Scrambling{} -> when (snakeHeadPos snakeHead == foodPos) $ do
        let ws = filter (/= tailWave foodStuff) [minBound..maxBound]
        listOpenCoords >>= randomNFromList 2 >>= traverse_ (newRandomFood ws)
        liftIO $ destroySprite (tailSprite foodStuff)
        destroy foodEty (Proxy @Food)
  -- Check for death (tail or edge)
  cmapM_ $ \(s@Snake{..}::Snake) -> do
    let V2 hx hy = snakeHeadPos snakeHead
    let oob = hx < 0 || hy < 0 || hx > worldBounds ^. _x || hy > worldBounds ^. _y
    let onTail = snakeHeadPos snakeHead `elem` snakeLocateTail s
    when (oob || onTail) $ cmap $ \(_::Screen) -> Dead
  cmap $ \case
    Scrambling 0 -> Nothing
    Scrambling n -> Just $ Scrambling $ pred n

