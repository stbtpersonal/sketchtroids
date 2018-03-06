module Fps where

data Fps = Fps { timeCount :: Double, frameCount :: Integer, timestamp :: Double }

new :: Fps
new = Fps { timeCount = 0, frameCount = 0, timestamp = 0 }