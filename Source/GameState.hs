module GameState where

import Haste.Graphics.Canvas as Canvas
import Fps
import Entity

data GameState = GameState { fps :: Fps, canvas :: Canvas, rotation :: Double, entities :: [Entity] }

new :: Canvas -> GameState
new canvas = GameState { fps = Fps.new, canvas = canvas, rotation = 0, entities = [] }