module Main where

import Control.Monad
import Haste.DOM as DOM
import Haste.Graphics.AnimationFrame as AnimationFrame
import Haste.Graphics.Canvas as Canvas
import GameState
import Fps

nativeWidth :: Double
nativeWidth = 800

nativeHeight :: Double
nativeHeight = 600

main :: IO ()
main = do
    divElement <- DOM.newElem "div"
    DOM.setAttr divElement "id" "text"
    DOM.appendChild DOM.documentBody divElement

    canvas <- Main.createCanvas
    Just contentElement <- DOM.elemById "content"
    DOM.appendChild contentElement canvas

    let initialState = GameState.new canvas
    AnimationFrame.requestAnimationFrame $ Main.mainLoop initialState
    return ()

createCanvas :: IO Canvas
createCanvas = do
    canvasElement <- DOM.newElem "canvas"
    DOM.setAttr canvasElement "id" "canvas"
    DOM.setAttr canvasElement "width" $ show Main.nativeWidth
    DOM.setAttr canvasElement "height" $ show Main.nativeHeight
    Just canvas <- DOM.fromElem canvasElement
    return canvas

mainLoop :: GameState -> AnimationFrame.HRTimeStamp -> IO ()
mainLoop state timestamp = do
    let fps = GameState.getFps state
    let (updatedFps, deltaTime, fpsToDisplay) = Main.updateFps fps timestamp

    let rotation = GameState.getRotation state
    let updatedRotation = rotation + (0.0001 * deltaTime)

    when (fpsToDisplay >= 0) $ do
        Main.render (GameState.getCanvas state) fpsToDisplay updatedRotation

    let newState = state { _fps = updatedFps, _rotation = updatedRotation }

    AnimationFrame.requestAnimationFrame $ Main.mainLoop newState
    return ()

updateFps :: Fps -> Double -> (Fps, Double, Integer)
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
        (updatedFps, deltaTime, fpsToDisplay)

render :: Canvas -> Integer -> Double -> IO ()
render canvas fps rotation = do
    Canvas.render canvas $ do 
        Main.background
        Main.square rotation
        Main.text fps
    return ()

background :: Picture ()
background = 
    let 
        shape = Canvas.rect (0, 0) (Main.nativeWidth, Main.nativeHeight)
        filledShape = Canvas.fill shape
        coloredShape = Canvas.color (Canvas.RGB 255 255 255) filledShape
    in
        coloredShape

square :: Double -> Picture ()
square rotation =
    let
        shape = Canvas.rect (-25, -25) (25, 25)
        stroked = Canvas.stroke shape
        colored = Canvas.color (Canvas.RGB 255 0 0) stroked
        rotated = Canvas.rotate rotation colored
        translated = Canvas.translate (Main.nativeWidth / 2, Main.nativeHeight / 2) rotated
    in
        translated

text :: Integer -> Picture ()
text fps =
    let
        text = Canvas.text (5, 22.5) $ "FPS: " ++ show fps
        fonted = Canvas.font "20px sans-serif" text
    in
        fonted