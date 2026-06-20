{-# LANGUAGE RecordWildCards #-}
module MJ626.BGM where

import MJ626.Env
import MJ626.World
import MJ626.Jfxr.JSFFI qualified as Jfxr
import Apecs
import Data.Foldable (traverse_)

switchBGM :: HasEnv => Screen -> System World ()
switchBGM screen = openEnv $ \Env{..} -> do
  Apecs.get global >>= (liftIO . traverse_ Jfxr.stopAudioBSN . bgmAudio)

  let toPlay = case screen of
        Title -> Nothing

  bgm <- liftIO $ traverse (Jfxr.loopAudioBuffer envAudio) toPlay

  Apecs.set global $ BGM bgm

