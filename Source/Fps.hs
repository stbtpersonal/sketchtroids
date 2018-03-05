module Fps where

data Fps = Fps { _timeCount :: Double, _frameCount :: Integer, _timestamp :: Double }

getTimeCount :: Fps -> Double
getTimeCount (Fps { _timeCount = timeCount }) = timeCount

getFrameCount :: Fps -> Integer
getFrameCount (Fps { _frameCount = frameCount }) = frameCount

getTimestamp :: Fps -> Double
getTimestamp (Fps { _timestamp = timestamp }) = timestamp

defaultValue :: Fps
defaultValue = Fps { _timeCount = 0, _frameCount = 0, _timestamp = 0 }