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
  const isMobile = ('ontouchstart' in window) || (navigator.maxTouchPoints > 0);

  const tags = {
    pink: { fill: 'hotpink' }
  };
  const scramble = '<pink>"Scramble"</pink>'
  const goal = `
GOAL:
- Collect waves to grow your signal-chain.
- Match 3 waves to clear them from your signal-chain
- Don't collide with your signal-chain or the wall!
- ${scramble} waves you don't want into new ones.
`;
  var controls;
  if (isMobile) {
    controls = `
CONTROLS:
Mobile:
- Swipe to Change Direction
- Double-tap to ${scramble}
`;
  } else {
    controls = `
CONTROLS:
Keyboard:
- WASD or Arrow Keys to Change Direction
- Space to ${scramble}
`;
  }

  var start;
  if (isMobile) {
    start = 'Double-tap to play';
  } else {
    start = 'Press Enter to play';
  }

  const msg = `
${goal}

${controls}

${start}
`;

  const titleStyle = new PIXI.TextStyle({
    fontFamily: 'PressStart2P',
    fontSize: 12,
    fill: 'white',
    wordWrap: true,
    wordWrapWidth: 300,
    align: 'left',
    tagStyles: tags
  });
  const txt = new PIXI.HTMLText({
    text: msg,
    style: titleStyle
  });
  txt.anchor.set(0.5)
  return txt;
  """
  newTutorialText :: IO Pixi.Text

foreign import javascript unsafe "$1.onViewUpdate()"
  onViewUpdate :: Pixi.Text -> IO ()

textVisible :: MonadIO m => Pixi.Text -> m ()
textVisible t = liftIO $ setProperty "visible" t (boolAsVal True)

textInvisible :: MonadIO m => Pixi.Text -> m ()
textInvisible t = liftIO $ setProperty "visible" t (boolAsVal False)
