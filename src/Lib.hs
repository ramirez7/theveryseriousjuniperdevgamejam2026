{-# LANGUAGE MultilineStrings #-}
-- | WebAssembly/JavaScript FFI bindings for PIXI.js
--
-- This module provides Haskell bindings to interact with PIXI.js, a 2D WebGL
-- renderer. It includes functions for creating and managing PIXI applications,
-- sprites, assets, and various utility functions for JavaScript interop.

--
-- All functions use the GHC WebAssembly backend's JavaScript FFI capabilities
-- to bridge between Haskell and JavaScript code running in the browser.
module Lib where

import GHC.Wasm.Prim

import Data.String (IsString(..))
import Data.Coerce

import Pixi.Types qualified as Pixi
-- | A JavaScript function represented as a JSVal.
-- This type alias is used to represent callable JavaScript functions
-- that can be passed between Haskell and JavaScript.
type JSFunction = JSVal

type IsJSVal a = Coercible a JSVal

foreign import javascript unsafe "Math.random()"
  jsRandom :: IO Float

-- *****************************************************************************
-- * PIXI.js Application Functions
-- *****************************************************************************

-- | Creates a new PIXI.js Application instance.
-- This creates a new PIXI Application object without initializing it.
-- Use 'initApp' to initialize the application with configuration options.
foreign import javascript unsafe "new PIXI.Application()"
   newApp :: IO Pixi.Application

foreign import javascript unsafe "new PIXI.Application({ width: $1, height: $2 })"
   newAppSized :: Int -> Int -> IO Pixi.Application

foreign import javascript unsafe
  """
  function resize() {
    const app = $1;
    const appWidth = app.screen.width;
    const appHeight = app.screen.height;
    console.log("wiw", window.innerWidth, "aw", appWidth, "wih", window.innerHeight, "ah", appHeight);
    const scale = Math.min(window.innerWidth / appWidth, window.innerHeight / appHeight);
    console.log("scale", scale);
    app.stage.scale.set(scale);
    app.stage.position.set(
        (window.innerWidth - appWidth * scale) / 2,
        (window.innerHeight - appHeight * scale) / 2
    );
    app.renderer.resize(window.innerWidth, window.innerHeight);
}
  window.addEventListener('resize', resize);
  resize();
  """
  resizeAppToScreen :: Pixi.Application -> IO ()

-- | Creates a new PIXI.js Text object with the specified text and fill color.
--
-- @param text The text content to display
-- @param fillColor The fill color as a JavaScript string (e.g., "white", "#FF0000")
-- @return A new Text object
foreign import javascript unsafe "new PIXI.Text({text: $1, style: {fill: $2 }})"
   newText :: JSString -> JSString -> IO Pixi.Text

foreign import javascript unsafe
  """
  new PIXI.Text({
    text: $1,
    style: {
      fill: $2,
      fontFamily: $3,
      fontSize: $4,
      align: $5
    }
  })
  """
   newTextCustom :: JSString -> JSString -> JSString -> Int -> JSString -> IO Pixi.Text

-- | Initializes a PIXI.js Application with the given background color.
--
-- This function uses a safe import because it needs to await the result of
-- the init function. The init function has side-effects on the app object,
-- which we need to capture.
--
-- @param app The PIXI Application object to initialize
-- @param backgroundColor The background color as a JavaScript string (e.g., "0x000000")
-- @return The initialized application object
foreign import javascript safe
 """
  const r = await $1.init({background: $2, resizeTo: window});
  return $1
 """
 initApp :: JSVal -> JSString -> IO JSVal

-- TODO: JSFFI is weird with await..why do we need to do this return stuff?
foreign import javascript safe
  """
  const r = await $1.init({width: $2, height: $3, antialias: false, autoDensity: true})
  return $1
  """
  initAppSized :: Pixi.Application -> Int -> Int -> IO Pixi.Application

-- | Initializes a PIXI.js Application with the given background color and resizes it to a target element.
--
-- This function uses a safe import because it needs to await the result of
-- the init function. The init function has side-effects on the app object,
-- which we need to capture.
--
-- @param app The PIXI Application object to initialize
-- @param backgroundColor The background color as a JavaScript string (e.g., "0x000000")
-- @param targetSelector The CSS selector for the target element to resize to (e.g., "#canvas-container")
-- @return The initialized application object
foreign import javascript safe
 """
  const r = await $1.init({background: $2, resizeTo: document.querySelector($3)});
  return $1
  """
    initAppInTarget :: Pixi.Application -> JSString -> JSString -> IO Pixi.Application


-- | Appends the application's canvas to the document body.
-- This makes the PIXI.js canvas visible in the browser.
--
-- @param app The PIXI Application object whose canvas should be appended
foreign import javascript unsafe "document.body.appendChild($1.canvas)"
    appendCanvas :: Pixi.Application -> IO ()


-- | Appends the application's canvas to a target element specified by a CSS selector.
-- This makes the PIXI.js canvas visible in the specified target element.
--
-- @param targetSelector The CSS selector for the target element (e.g., "#canvas-container")
-- @param app The PIXI Application object whose canvas should be appended
foreign import javascript unsafe "document.querySelector($1).appendChild($2.canvas)"
    appendToTarget :: JSString -> Pixi.Application -> IO ()

foreign import javascript unsafe "PIXI.TexturePool.textureOptions.scaleMode = 'nearest'"
  setScalingNearestNeighbor :: IO ()

-- *****************************************************************************
-- * Console Logging
-- *****************************************************************************

-- | Logs a JavaScript value to the browser console.
--
-- @param val The JavaScript value to log
foreign import javascript unsafe "console.log($1)"
    consoleLogVal :: JSVal -> IO ()

-- | Logs a Haskell value to the browser console by converting it to a string.
-- This is a convenience function that combines 'show', 'toJSString', 'stringAsVal',
-- and 'consoleLogVal' to log any Show instance.
--
-- @param a A value of any type that has a Show instance
consoleLogShow :: Show a => a -> IO ()
consoleLogShow = consoleLogVal . stringAsVal . toJSString . show

consoleLogStr :: String -> IO ()
consoleLogStr = consoleLogVal . stringAsVal . toJSString
-- *****************************************************************************
-- * Asset Loading
-- *****************************************************************************

-- | Loads a PIXI.js asset (texture) asynchronously.
-- This function uses a safe import because it needs to await the asset loading.
--
-- @param path The path to the asset as a JavaScript string
-- @return The loaded texture as a JSVal
foreign import javascript safe "const texture = await Assets.load($1); return texture"
    loadTexture :: JSString -> IO Pixi.Texture

-- *****************************************************************************
-- * Sprite Functions
-- *****************************************************************************

-- | Creates a new PIXI.js Sprite from a texture.
--
-- @param texture The texture to use for the sprite
-- @return A new Sprite object
foreign import javascript unsafe "new PIXI.Sprite($1)"
    newSprite :: Pixi.Texture -> IO Pixi.Sprite

foreign import javascript unsafe "$1.destroy()"
  destroySprite :: Pixi.Sprite -> IO ()

foreign import javascript unsafe """
new PIXI.TilingSprite({
  texture: $1,
  width: $2,
  height: $3,
})
"""
  newTilingSprite :: Pixi.Texture -> Int -> Int -> IO Pixi.Sprite

foreign import javascript unsafe "new PIXI.Container()"
  newContainer :: IO Pixi.Container

foreign import javascript unsafe "$1.addChild($2)"
    addContainerChild' :: Pixi.Container -> JSVal -> IO ()

addContainerChild :: IsJSVal a => Pixi.Container -> a -> IO ()
addContainerChild app a = addContainerChild' app (coerce a)

-- | Gets a base texture from PIXI's built-in texture cache.
--
-- Common texture names include "WHITE" for a white rectangle texture.
--
-- @param textureName The name of the texture (e.g., "WHITE")
-- @return The texture object
foreign import javascript unsafe "PIXI.Texture[$1]"
   baseTexture :: JSString -> IO Pixi.Texture

-- | Plays a default blip sound effect.
--
-- Uses the default frequency, duration, and volume settings.
foreign import javascript unsafe "blip()"
    blip :: IO ()

-- | Plays a blip sound effect with custom parameters.
--
-- @param frequency Frequency in Hz
-- @param duration Duration in milliseconds
-- @param volume Volume level (0.0 to 1.0)
foreign import javascript unsafe "blip($1, $2, $3)"
    blipWithArgs :: Float -> Float -> Float -> IO ()

-- | Plays a blip sound effect with a custom frequency.
--
-- Uses default duration and volume settings.
--
-- @param frequency Frequency in Hz
foreign import javascript unsafe "blip($1)"
    blipWithFreq :: Float -> IO ()


-- | Sets the anchor point of a sprite.
-- The anchor point determines the point around which transformations are applied.
-- A value of 0.5 means the anchor is at the center of the sprite.
--
-- @param sprite The sprite whose anchor should be set
-- @param anchorValue The anchor value (typically 0.0 to 1.0)
foreign import javascript unsafe "$1.anchor.set($2)"
    setAnchor :: JSVal -> Float -> IO ()

setTextAnchor :: Pixi.Text -> Float -> IO ()
setTextAnchor t f = setAnchor (coerce t) f

setSpriteAnchor :: Pixi.Sprite -> Float -> IO ()
setSpriteAnchor t f = setAnchor (coerce t) f

-- | Adds a child object to the application's stage.
-- This makes the child visible in the renderer.
--
-- @param app The PIXI Application object
-- @param child The child object (e.g., a Sprite) to add to the stage
foreign import javascript unsafe "$1.stage.addChild($2)"
    addChild' :: Pixi.Application -> JSVal -> IO ()

addChild :: IsJSVal a => Pixi.Application -> a -> IO ()
addChild app a = addChild' app (coerce a)

-- *****************************************************************************
-- * Function Calling and Ticker
-- *****************************************************************************

-- | Calls a JavaScript function with a single argument.
--
-- @param func The JavaScript function to call
-- @param arg The argument to pass to the function
-- @return The return value of the function call
foreign import javascript unsafe "$1($2)"
  callFunction :: JSFunction -> JSVal -> IO JSVal

-- | Adds a function to the PIXI.js ticker.
-- The ticker calls the function on each frame update, allowing for animation
-- and continuous updates.
--
-- @param app The PIXI Application object
-- @param func The function to call on each tick
foreign import javascript unsafe "$1.ticker.add($2)"
    addTicker :: Pixi.Application -> JSFunction -> IO ()

-- | Creates a new PIXI.js Ticker instance.
--
-- A ticker is used to call functions repeatedly at a specified rate.
-- Use 'startTicker' to begin ticking, and 'callAddTicker' to add callbacks.
--
-- @return A new Ticker object
foreign import javascript unsafe "new PIXI.Ticker()"
    newTicker :: IO Pixi.Ticker

-- | Adds a callback function to a ticker.
--
-- The callback will be called on each tick with the ticker object as an argument.
--
-- @param ticker The ticker to add the callback to
-- @param func The function to call on each tick
foreign import javascript unsafe "$1.add($2)"
    callAddTicker :: Pixi.Ticker -> JSFunction -> IO ()

-- | Starts a ticker, causing it to begin calling its callbacks.
--
-- @param ticker The ticker to start
foreign import javascript unsafe "$1.start()"
    startTicker :: Pixi.Ticker -> IO ()

-- *****************************************************************************
-- * JavaScript Interop Utilities
-- *****************************************************************************

-- | Converts a Haskell function to a JavaScript function.
-- This creates a JavaScript function that can be called from JavaScript code
-- and will execute the provided Haskell function.
--
-- @param hsFunc A Haskell function that takes a JSVal and returns an IO JSVal
-- @return A JavaScript function that can be passed to JavaScript code
foreign import javascript "wrapper"
  jsFuncFromHs :: (JSVal -> IO JSVal) -> IO JSVal

-- | Converts a Haskell function to a JavaScript function that does not return a value.
jsFuncFromHs_ :: IsJSVal a => (a -> IO ()) -> IO JSVal
jsFuncFromHs_ func =
    jsFuncFromHs (\val -> do
        func (coerce val)
        return (coerce val)
    )

-- | Gets a property from a JavaScript object.
--
-- @param propName The name of the property to get
-- @param obj The JavaScript object
-- @return The value of the property
foreign import javascript "$2[$1]"
  getProperty' :: JSString -> JSVal -> IO JSVal

getProperty :: IsJSVal a => JSString -> a -> IO JSVal
getProperty p v = getProperty' p (coerce v)

-- | Gets a property from a JavaScript object by a list of keys.
getPropertyKey :: IsJSVal a => [JSString] -> a -> IO JSVal
getPropertyKey keys obj =
    case keys of
        [] -> return (coerce obj)
        (k:ks) -> do
            tmp <- getProperty k obj
            getPropertyKey ks tmp

-- | Sets a property on a JavaScript object by a list of keys.
setPropertyKey ::  IsJSVal a => [JSString] -> a -> JSVal -> IO ()
setPropertyKey keys obj value =
    case keys of
        [k] -> setProperty k obj value
        (k:ks) -> do tmp <- getProperty k obj
                     setPropertyKey ks tmp value
                     setProperty k obj tmp


-- | Sets a property on a JavaScript object.
--
-- @param propName The name of the property to set
-- @param obj The JavaScript object
-- @param value The value to set
foreign import javascript "$2[$1] = $3"
  setProperty' ::  JSString -> JSVal -> JSVal -> IO ()

setProperty :: (IsJSVal a, IsJSVal b) => JSString -> a -> b -> IO ()
setProperty p o v = setProperty' p (coerce o) (coerce v)

-- *****************************************************************************
-- * Type Conversion Functions
-- *****************************************************************************

-- | Converts a JavaScript value to a Float.
-- This is an unsafe conversion - the value must be a number.
foreign import javascript "$1"
  valAsFloat :: JSVal -> Float

-- | Converts a JavaScript value to an Int.
-- This is an unsafe conversion - the value must be a number.
foreign import javascript "$1"
  valAsInt :: JSVal -> Int

-- | Converts a JavaScript value to a Bool.
-- This is an unsafe conversion - the value must be a boolean.
foreign import javascript "$1"
  valAsBool :: JSVal -> Bool

-- | Converts a JavaScript value to a JSString.
-- This is an unsafe conversion - the value must be a string.
foreign import javascript "$1"
  valAsString :: JSVal -> JSString

-- | Converts a Float to a JavaScript value.
foreign import javascript "$1"
  floatAsVal :: Float -> JSVal

-- | Converts an Int to a JavaScript value.
foreign import javascript "$1"
  intAsVal :: Int -> JSVal

-- | Converts a Bool to a JavaScript value.
foreign import javascript "$1"
  boolAsVal :: Bool -> JSVal

-- | Converts a JSString to a JavaScript value.
foreign import javascript "$1"
  stringAsVal :: JSString -> JSVal

-- | Increments a property on a JavaScript object.
-- This is equivalent to @obj[propName] += value@ in JavaScript.
--
-- @param propName The name of the property to increment
-- @param obj The JavaScript object
-- @param value The value to add to the property
foreign import javascript "$2[$1] += $3"
  incrementProperty' :: JSString -> JSVal -> JSVal -> IO ()

incrementProperty :: IsJSVal a => JSString -> a -> JSVal -> IO ()
incrementProperty s o v = incrementProperty' s (coerce o) v

-- | Instance allowing JSString to be created from Haskell strings using
-- string literals or 'fromString'. This enables convenient syntax like
-- @\"hello\" :: JSString@.
instance IsString JSString where
    fromString = toJSString


-- | Adds an event listener to a JavaScript object.
--
-- @param event The event to listen for (e.g., "click")
-- @param obj The JavaScript object to add the event listener to
-- @param listener The function to call when the event occurs
foreign import javascript unsafe "$2.on($1, $3)"
  addEventListener' :: JSString -> JSVal -> JSFunction -> IO ()

addEventListener :: IsJSVal a => JSString -> a -> JSFunction -> IO ()
addEventListener e o l = addEventListener' e (coerce o) l

foreign import javascript unsafe "window.addEventListener($1, $2)"
  addWindowEventListener :: JSString -> JSFunction -> IO ()

foreign import javascript unsafe "document.addEventListener($1, $2)"
  addDocumentEventListener :: JSString -> JSFunction -> IO ()

addDocumentEventListenerHs :: JSString -> (JSVal -> IO ()) -> IO ()
addDocumentEventListenerHs e f = jsFuncFromHs_ f >>= addDocumentEventListener e
foreign import javascript unsafe "$1.touches[0]"
  getEventTouch :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.changedTouches[0]"
  getEventChangedTouch :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.changedTouches.length"
  getEventChangedTouchNum :: JSVal -> IO Int

foreign import javascript unsafe "new Date().getTime()"
  jsGetTime :: IO JSVal

foreign import javascript unsafe "('ontouchstart' in window) || (navigator.maxTouchPoints > 0)"
  isTouchDevice :: IO Bool
