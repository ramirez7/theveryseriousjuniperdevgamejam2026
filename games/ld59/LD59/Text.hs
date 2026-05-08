{-# LANGUAGE MultilineStrings #-}
module LD59.Text where

import GHC.Wasm.Prim
import Pixi.Types qualified as Pixi

foreign import javascript unsafe
  """
  const gameStyle = new PIXI.TextStyle({
    fontFamily: 'Courier New',
    fontSize: 24,
    fill: 'white',
    stroke: {
        color: 'black',
        width: 5
    },
/*
    dropShadow: {
        color: 'black',
        blur: 4,
        distance: 6,
        angle: Math.PI / 6
    },*/
    wordWrap: true,
    wordWrapWidth: 440,
    lineHeight: 40,
    align: 'center'
  });

  const txt = new PIXI.Text({
    text: $1,
    style: gameStyle
  });
  txt.anchor.set(0.5);
  return txt;
  """
  newScoreText :: JSString -> IO Pixi.Text
