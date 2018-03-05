module GameState where

import Fps

data GameState = GameState { _fps :: Fps }

getFps :: GameState -> Fps
getFps (GameState { _fps = fps }) = fps

defaultValue :: GameState
defaultValue = GameState { _fps = Fps.defaultValue }