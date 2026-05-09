{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}

module LD59.Snake where

import Data.Map qualified as Map
import Linear.V2
import Data.Foldable (toList, for_, traverse_)
import Data.List (scanl')
import Data.List.Extra (dropEnd)
import Control.Lens
import Safe (lastMay)
import Apecs
import GHC.Generics
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NEL
import LD59.Dir
import Data.Maybe (isJust)
import Data.List (nub)

data SnakeHead h = SnakeHead
  { snakeHeadVal :: h
  , snakeHeadPos :: V2 Int
  , snakeHeadDir :: Dir
  } deriving stock (Show, Generic, Functor, Foldable, Traversable)

data SnakeTailSeg t = SnakeTailSeg
  { snakeTailVal :: t
  , snakeTailDir :: Dir
  } deriving stock (Show, Generic, Functor, Foldable, Traversable)

newtype SnakeTail t = SnakeTail { snakeTailSegs :: [SnakeTailSeg t] }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

data SnakeF h t = Snake
  { snakeHead :: SnakeHead h
  , snakeTail :: SnakeTail t
  , snakeStomachDir :: Dir
  } deriving stock (Show, Generic)

snakeLocateTail :: SnakeF h t -> [V2 Int]
snakeLocateTail Snake{..} =
  -- Need to drop both ends here due to how scanl works
  dropEnd 1 $ drop 1 $ scanl' (\p dir -> p + (negate $ dirV2 dir)) (snakeHeadPos snakeHead) (snakeHeadDir snakeHead : fmap snakeTailDir (snakeTailSegs snakeTail))

snakeMatch :: Eq a => (t -> a) -> SnakeF h t -> (SnakeF h t, Maybe (SnakeTail t, a))
snakeMatch f Snake{..} =
  let revtail = reverse $ snakeTailSegs snakeTail
      (tip, rest) = (take 3 revtail, drop 3 revtail)
      nubbedTip = nub $ fmap (f . snakeTailVal) tip
      remakeTail = SnakeTail . reverse
      match = case (tip, nubbedTip) of
        ([_,_,_], [a]) -> Just (remakeTail tip, a)
        _ -> Nothing
      newTail = if isJust match then remakeTail rest else snakeTail
  in (Snake{ snakeTail = newTail, .. }, match)

-- This doesn't stop you from moving into the tail
snakeMove :: Dir -> SnakeF h t -> SnakeF h t
snakeMove dir Snake{..} =
  let tailDirs = fmap snakeTailDir (snakeTailSegs snakeTail)
      snakeDirs = snakeHeadDir snakeHead :| tailDirs
  in Snake
     { snakeHead = snakeHead
                   & #snakeHeadPos %~ (+ dirV2 dir)
                   & #snakeHeadDir .~ dir
     , snakeTail = SnakeTail $ zipWith (\dir' SnakeTailSeg{..} -> SnakeTailSeg{snakeTailDir = dir', ..}) (toList snakeDirs) (snakeTailSegs snakeTail)
     , snakeStomachDir = maybe (snakeHeadDir snakeHead) snakeTailDir (lastMay (snakeTailSegs snakeTail))
     }

snakeEat :: (food -> t) -> food -> SnakeF h t -> SnakeF h t
snakeEat digest food Snake{..} =
  Snake
  { snakeTail = snakeTail & #snakeTailSegs %~ flip snoc (SnakeTailSeg (digest food) snakeStomachDir)
  , ..
  }
  
instance Component (SnakeF h t) where type Storage (SnakeF h t) = Unique (SnakeF h t)

exampleSnake :: SnakeF () ()
exampleSnake = Snake
  { snakeHead = SnakeHead () (V2 3 3) DOWN
  , snakeTail = SnakeTail $ SnakeTailSeg () <$> [DOWN, DOWN, LEFT]
  , snakeStomachDir = LEFT
  }

printSnake2D :: SnakeF h t -> IO ()
printSnake2D s@Snake{..} = do
  let ps = mconcat
        [[(snakeHeadPos snakeHead, 'H')]
        ,fmap (,'T') (snakeLocateTail s)
        ]
  let pmap = Map.fromList ps
  let maxX = maximum $ fmap (view _x . fst) ps
  let maxY = maximum $ fmap (view _y . fst) ps
  let coords  = fmap (\y -> fmap (\x -> Map.lookup (V2 x y) pmap) [0..maxX]) [0..maxY]

  putStr "  "
  traverse_ (putStr . (++ " ") . show) [0..maxX]
  putStrLn ""
  
  for_ (zip [(0 :: Int)..] coords) $ \(rowNo, row) -> do
    putStr (show rowNo ++ " ")
    for_ row $ \c ->
      putStr $ maybe "_ " (:" ") c
    putStrLn ""

{-
ghci> printSnake2D (exampleSnake)                 
  0 1 2 3 4 
0 _ _ _ T T 
1 _ _ _ T _ 
2 _ _ _ T _ 
3 _ _ _ H _ 
ghci> printSnake2D (snakeMove DOWN $ exampleSnake)
  0 1 2 3 
0 _ _ _ T 
1 _ _ _ T 
2 _ _ _ T 
3 _ _ _ T 
4 _ _ _ H 
ghci> printSnake2D (snakeEat () $ snakeMove DOWN $ exampleSnake)
  0 1 2 3 4 
0 _ _ _ T T 
1 _ _ _ T _ 
2 _ _ _ T _ 
3 _ _ _ T _ 
4 _ _ _ H _
-}
