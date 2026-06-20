module MJ626.Random where

import Lib (jsRandom)
import Control.Monad.IO.Class

randomFromList :: MonadIO m => [a] -> m a
randomFromList = fmap fst . randomIdxFromList

randomNFromList :: MonadIO m => Int -> [a] -> m [a]
randomNFromList 0 _ = pure []
randomNFromList _ [] = pure []
randomNFromList n xs = do
  (a, i) <- randomIdxFromList xs
  let xs' = filter (\(_, j) -> i /= j) (xs `zip` [0..])
  (a :) <$> randomNFromList (n - 1) (fmap fst xs')

shuffleList :: MonadIO m => [a] -> m [a]
shuffleList xs = randomNFromList (length xs) xs

randomIdxFromList :: MonadIO m => [a] -> m (a, Int)
randomIdxFromList [] = error "randomFromList ERROR: empty list"
randomIdxFromList xs = do
  n <- liftIO jsRandom
  pure $ (xs `zip` [0..]) !! floor (fromIntegral (length xs) * n)
