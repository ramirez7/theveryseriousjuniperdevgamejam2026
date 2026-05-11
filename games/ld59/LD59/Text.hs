{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module LD59.Text where

import GHC.Wasm.Prim
import Pixi.Types qualified as Pixi
import Control.Monad.IO.Class
import Lib

foreign import javascript safe
  """
  await Assets.load({
    src: './PressStart2P-Regular.ttf',
    data: { family: 'PressStart2P' }
  });
  """
  initGameFonts :: IO ()

foreign import javascript unsafe
  """
  const gameStyle = new PIXI.TextStyle({
    fontFamily: 'PressStart2P',
    fontSize: 24,
    fill: 'white',
    wordWrap: true,
    wordWrapWidth: 440,
    lineHeight: 40,
    align: 'center'
  });

  const txt = new PIXI.Text({
    text: '',
    style: gameStyle
  });
  txt.anchor.set(0.5);
  return txt;
  """
  newScoreText :: IO Pixi.Text

foreign import javascript unsafe
  """
  const titleStyle = new PIXI.TextStyle({
    fontFamily: 'PressStart2P',
    fontSize: 36,
    fill: 'white',
    wordWrap: true,
    wordWrapWidth: 440,
    lineHeight: 40,
    align: 'center'
  });
  const txt = new PIXI.Text({
    text: 'Digital Signal Puzzler',
    style: titleStyle
  });
  txt.anchor.set(0.5)
  return txt;
  """
  newTitleText :: IO Pixi.Text

foreign import javascript unsafe
  """
  const style = new PIXI.TextStyle({
    fontFamily: 'PressStart2P',
    fontSize: 36,
    fill: 'red',
    wordWrap: true,
    wordWrapWidth: 440,
    lineHeight: 40,
    align: 'center'
  });
  const txt = new PIXI.Text({
    text: 'GAME OVER',
    style: style
  });
  txt.anchor.set(0.5)
  return txt;
  """
  newGameOverText :: IO Pixi.Text

foreign import javascript unsafe
  """
  const isMobile = ('ontouchstart' in window) || (navigator.maxTouchPoints > 0);
  var msg;
  if (isMobile) {
    msg = 'Double-tap to play';
  } else {
    msg = 'Press Enter to play';
  }
  const titleStyle = new PIXI.TextStyle({
    fontFamily: 'PressStart2P',
    fontSize: 16,
    fill: 'white',
    wordWrap: true,
    wordWrapWidth: 440,
    lineHeight: 40,
    align: 'center'
  });
  const txt = new PIXI.Text({
    text: msg,
    style: titleStyle
  });
  txt.anchor.set(0.5)
  return txt;
  """
  newPressStartText :: IO Pixi.Text

foreign import javascript unsafe
  """
  var msg = `
  GOAL:
  - Collect waves to grow your signal-chain.
  - Match 3 waves to clear them from your signal-chain
  - Don't collide with your signal-chain or the wall!
  - "Scramble" waves you don't want into new ones.

  CONTROLS:
  Keyboard:
  - WASD or Arrow Keys to Change Direction
  - Space to "Scramble"

  Mobile:
  - Swipe to Change Direction
  - Double-tap to "Scramble"


  `
  const isMobile = ('ontouchstart' in window) || (navigator.maxTouchPoints > 0);
  if (isMobile) {
    msg += 'Double-tap to play';
  } else {
    msg += 'Press Enter to play';
  }

  const titleStyle = new PIXI.TextStyle({
    fontFamily: 'PressStart2P',
    fontSize: 12,
    fill: 'white',
    wordWrap: true,
    wordWrapWidth: 300,
    align: 'left'
  });
  const txt = new PIXI.Text({
    text: msg,
    style: titleStyle
  });
  txt.anchor.set(0.5)
  return txt;
  """
  newTutorialText :: IO Pixi.Text

textVisible :: MonadIO m => Pixi.Text -> m ()
textVisible t = liftIO $ setProperty "visible" t (boolAsVal True)

textInvisible :: MonadIO m => Pixi.Text -> m ()
textInvisible t = liftIO $ setProperty "visible" t (boolAsVal False)
