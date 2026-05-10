{-# LANGUAGE RecordWildCards #-}
module LD59.BGM where

import LD59.Env
import LD59.World
import LD59.Jfxr.JSFFI qualified as Jfxr
import Apecs
import Data.Foldable (traverse_)
import LD59.Art

switchBGM :: HasEnv => Screen -> System World ()
switchBGM screen = openEnv $ \Env{..} -> do
  Apecs.get global >>= (liftIO . traverse_ Jfxr.stopAudioBSN . bgmAudio)

  let toPlay = case screen of
        Title -> Nothing
        Playing -> Just (artPlayingBGM envArt)
        Tutorial -> Just (artGameoverBGM envArt)
        Dead -> Just (artGameoverBGM envArt)

  bgm <- liftIO $ traverse (Jfxr.loopAudioBuffer envAudio) toPlay

  Apecs.set global $ BGM bgm

