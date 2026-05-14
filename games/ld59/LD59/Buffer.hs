{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module LD59.Buffer where

import GHC.TypeLits
import Control.Lens (snoc)
import Data.Proxy

newtype Buffer (n :: Natural) a = Buffer [a] deriving stock (Show)

buffer :: forall n a. KnownNat n => a -> Buffer n a -> Buffer n a
buffer x (Buffer xs) =
  let maxSize = fromIntegral $ natVal (Proxy :: Proxy n)
  in if length xs >= maxSize then Buffer xs else Buffer (snoc xs x)

bufferWhenLast :: forall n a. KnownNat n => (a -> Bool) -> a -> Buffer n a -> Buffer n a
bufferWhenLast p x b = case peekbufferLast b of
  Nothing -> buffer x b
  Just y -> if p y then buffer x b else b
  
unbuffer :: forall n a. KnownNat n => Buffer n a -> (Maybe a, Buffer n a)
unbuffer = \case
  Buffer [] -> (Nothing, Buffer [])
  Buffer (x:xs) -> (Just x, Buffer xs)

unbufferWhen :: forall n a. KnownNat n => (a -> Bool) -> Buffer n a -> (Maybe a, Buffer n a)
unbufferWhen p = \case
  Buffer [] -> (Nothing, Buffer [])
  Buffer (x:xs) | p x -> (Just x, Buffer xs)
  Buffer xs -> (Nothing, Buffer xs)

dropBufferWhile :: (a -> Bool) -> Buffer n a -> Buffer n a
dropBufferWhile p (Buffer xs) = Buffer (dropWhile p xs)

peekbuffer :: Buffer n a -> Maybe a
peekbuffer = \case
  Buffer [] -> Nothing
  Buffer (x:xs) -> Just x

peekbufferLast :: Buffer n a -> Maybe a
peekbufferLast = \case
  Buffer [] -> Nothing
  Buffer xs -> Just (last xs)
