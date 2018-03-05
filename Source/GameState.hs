module GameState where

import Haste.Graphics.Canvas as Canvas
import Fps

data GameState = GameState { _fps :: Fps, _canvas :: Canvas, _rotation :: Double }

getFps :: GameState -> Fps
getFps (GameState { _fps = fps }) = fps

getCanvas :: GameState -> Canvas
getCanvas (GameState { _canvas = canvas }) = canvas

getRotation :: GameState -> Double
getRotation (GameState { _rotation = rotation }) = rotation

new :: Canvas -> GameState
new canvas = GameState { _fps = Fps.defaultValue, _canvas = canvas, _rotation = 0 }