{-# LANGUAGE RecordWildCards #-}
module MJ626.BGM where

import MJ626.Env
import MJ626.ECS
import MJ626.Jfxr.JSFFI qualified as Jfxr
import Apecs
import Data.Foldable (traverse_)

switchBGM :: HasEnv => Scene -> System ECS ()
switchBGM scene = openEnv $ \Env{..} -> do
  Apecs.get global >>= (liftIO . traverse_ Jfxr.stopAudioBSN . bgmAudio)

  let toPlay = case scene of
        Title -> Nothing

  bgm <- liftIO $ traverse (Jfxr.loopAudioBuffer envAudio) toPlay

  Apecs.set global $ BGM bgm

