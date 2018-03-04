module Fps where

data Fps = Fps { _timeCount :: Integer, _frameCount :: Integer }

getTimeCount :: Fps -> Integer
getTimeCount (Fps { _timeCount = timeCount }) = timeCount

setTimeCount :: Fps -> Integer -> Fps
setTimeCount fps timeCount = fps { _timeCount = timeCount }

getFrameCount :: Fps -> Integer
getFrameCount (Fps { _frameCount = frameCount }) = frameCount

setFrameCount :: Fps -> Integer -> Fps
setFrameCount fps frameCount = fps { _frameCount = frameCount }

defaultValue :: Fps
defaultValue = Fps { _timeCount = 0, _frameCount = 0 }