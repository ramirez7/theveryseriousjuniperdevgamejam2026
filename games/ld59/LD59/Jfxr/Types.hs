module LD59.Jfxr.Types where
import Data.Aeson as Ae
import GHC.Generics
import Data.Scientific
import Data.Char (toLower)
data JfxrDef = JfxrDef
  { jfxr_name :: String
  , jfxr_version :: Scientific
  , jfxrAmplification :: Scientific
  , jfxrAttack :: Scientific
  , jfxrBitCrush :: Scientific
  , jfxrBitCrushSweep :: Scientific
  , jfxrCompression :: Scientific
  , jfxrDecay :: Scientific
  , jfxrFlangerOffset :: Scientific
  , jfxrFlangerOffsetSweep :: Scientific
  , jfxrFrequency :: Scientific
  , jfxrFrequencyDeltaSweep :: Scientific
  , jfxrFrequencyJump1Amount :: Scientific
  , jfxrFrequencyJump1Onset :: Scientific
  , jfxrFrequencyJump2Amount :: Scientific
  , jfxrFrequencyJump2Onset :: Scientific
  , jfxrFrequencySweep :: Scientific
  , jfxrHarmonics :: Scientific
  , jfxrHarmonicsFalloff :: Scientific
  , jfxrHighPassCutoff :: Scientific
  , jfxrHighPassCutoffSweep :: Scientific
  , jfxrInterpolateNoise :: Bool
  , jfxrLowPassCutoff :: Scientific
  , jfxrLowPassCutoffSweep :: Scientific
  , jfxrNormalization :: Bool
  , jfxrRepeatFrequency :: Scientific
  , jfxrSampleRate :: Scientific
  , jfxrSquareDuty :: Scientific
  , jfxrSquareDutySweep :: Scientific
  , jfxrSustain :: Scientific
  , jfxrSustainPunch :: Scientific
  , jfxrTremoloDepth :: Scientific
  , jfxrTremoloFrequency :: Scientific
  , jfxrVibratoDepth :: Scientific
  , jfxrVibratoFrequency :: Scientific
  , jfxrWaveform :: String
  }
  deriving (Eq, Ord, Show, Read, Generic)

uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLower x : xs

aesonStrip'jfxr :: Ae.Options
aesonStrip'jfxr = Ae.defaultOptions { fieldLabelModifier = uncapitalize . drop 4 }

instance Ae.ToJSON JfxrDef where
  toJSON = genericToJSON aesonStrip'jfxr
  toEncoding = genericToEncoding aesonStrip'jfxr
instance Ae.FromJSON JfxrDef where
  parseJSON = genericParseJSON aesonStrip'jfxr
