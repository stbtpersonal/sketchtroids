module GameState where

import Fps

data GameState = GameState { _counter :: Integer, _fps :: Fps }

getCounter :: GameState -> Integer
getCounter (GameState { _counter = counter }) = counter

setCounter :: GameState -> Integer -> GameState
setCounter gameState counter = gameState { _counter = counter }

getFps :: GameState -> Fps
getFps (GameState { _fps = fps }) = fps

setFps :: GameState -> Fps -> GameState
setFps gameState fps = gameState { _fps = fps }

defaultValue :: GameState
defaultValue = GameState { _counter = 0, _fps = Fps.defaultValue }