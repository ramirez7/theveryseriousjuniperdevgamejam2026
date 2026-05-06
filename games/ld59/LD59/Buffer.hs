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

unbuffer :: forall n a. KnownNat n => Buffer n a -> (Maybe a, Buffer n a)
unbuffer = \case
  Buffer [] -> (Nothing, Buffer [])
  Buffer (x:xs) -> (Just x, Buffer xs)

peekbuffer :: Buffer n a -> Maybe a
peekbuffer = \case
  Buffer [] -> Nothing
  Buffer (x:xs) -> Just x
