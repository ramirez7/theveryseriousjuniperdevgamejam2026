{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
module MJ626.Sfx where

import MJ626.Env
import MJ626.Art
import MJ626.ECS
import Data.Function ((&))
import MJ626.Jfxr.Types
import MJ626.Jfxr.JSFFI
import Control.Monad.IO.Class
import Control.Monad ((>=>))
import Data.Scientific
import MJ626.Wave

baseSfx :: HasEnv => JfxrDef
baseSfx = openEnv $ \Env{..} -> artSinJfxr envArt

waveToJfxr :: Wave -> String
waveToJfxr = \case
  TRI -> "triangle"
  SIN -> "sine"
  SQUARE -> "square"
  SAW -> "sawtooth"
  TAN -> "tangent"

playJfxr :: MonadIO m => HasEnv => JfxrDef -> m ()
playJfxr = openEnv $ \Env{..} -> liftIO . (newClip >=> playClip envAudio)

withNote :: Note -> Octave -> JfxrDef -> JfxrDef
withNote n o d = d { jfxrFrequency = noteFreq n o }

withWaveform :: String -> JfxrDef -> JfxrDef
withWaveform wf d = d { jfxrWaveform = wf }

withWave :: Wave -> JfxrDef -> JfxrDef
withWave = withWaveform . waveToJfxr

withDecay :: Scientific -> JfxrDef -> JfxrDef
withDecay dl d = d { jfxrDecay = dl }

withSustain :: Scientific -> JfxrDef -> JfxrDef
withSustain dl d = d { jfxrSustain = dl }
data Note =
    C
  | Cs
  | D
  | Ds
  | E
  | F
  | Fs
  | G
  | Gs
  | A
  | As
  | B
  deriving (Eq, Ord, Show, Enum, Bounded)

type Octave = Int

noteFreq :: Note -> Octave -> Scientific
noteFreq n o = noteFreq4 n * 2 ^^ (o - 4)

noteFreq4 :: Note -> Scientific
noteFreq4 = \case
  C -> 261.63
  Cs -> 277.18
  D -> 293.66
  Ds -> 311.13
  E -> 329.63
  F -> 349.23
  Fs -> 369.99
  G -> 392
  Gs -> 415.30
  A -> 440
  As -> 466.16
  B -> 493.88
