module Main where

import Control.Monad
import Haste.DOM as DOM
import Haste.Graphics.AnimationFrame as AnimationFrame
import GameState
import Fps

main :: IO ()
main = do
    divElement <- DOM.newElem "div"
    DOM.setAttr divElement "id" "text"
    DOM.appendChild DOM.documentBody divElement

    let initialState = GameState.defaultValue
    AnimationFrame.requestAnimationFrame $ mainLoop initialState
    return ()

mainLoop :: GameState -> AnimationFrame.HRTimeStamp -> IO ()
mainLoop state timestamp = do
    let fps = GameState.getFps state
    let (updatedFps, fpsToDisplay) = updateFps fps timestamp

    when (fpsToDisplay >= 0) $ do
        Just divElement <- DOM.elemById "text"
        DOM.clearChildren divElement
        textElement <- DOM.newTextElem $ "FPS: " ++ show fpsToDisplay
        DOM.appendChild divElement textElement

    let newState = GameState { _fps = updatedFps }

    AnimationFrame.requestAnimationFrame $ mainLoop newState
    return ()

updateFps :: Fps -> Double -> (Fps, Integer)
updateFps fps timestamp =
    let
        previousTimestamp = Fps.getTimestamp fps
        deltaTime = timestamp - previousTimestamp
        candidateTimeCount = Fps.getTimeCount fps + deltaTime
        candidateFrameCount = Fps.getFrameCount fps + 1
        (updatedTimeCount, updatedFrameCount, fpsToDisplay)
            | candidateTimeCount < 1000 = (candidateTimeCount, candidateFrameCount, -1)
            | otherwise                 = (candidateTimeCount - 1000, 1, candidateFrameCount)
        updatedFps = Fps { _timeCount = updatedTimeCount, _frameCount = updatedFrameCount, _timestamp = timestamp }
    in
        (updatedFps, fpsToDisplay)
