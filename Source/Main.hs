module Main where

import Control.Monad
import Haste.DOM as DOM
import Haste.Graphics.AnimationFrame as AnimationFrame
import Haste.Graphics.Canvas as Canvas
import GameState
import Fps
import SpinningRectangle
import Point
import Entity

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

    let spinningRectangle1 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 50, Point.y = 50 }, SpinningRectangle.speed = 0.001 }
    let spinningRectangle2 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 150, Point.y = 50 }, SpinningRectangle.speed = 0.002 }
    let spinningRectangle3 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 50, Point.y = 150 }, SpinningRectangle.speed = 0.003 }
    let initialState = GameState.new canvas 
    AnimationFrame.requestAnimationFrame $ Main.mainLoop $ initialState { GameState.entities = [spinningRectangle1, spinningRectangle2, spinningRectangle3] }
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
    let fps = GameState.fps state
    let (updatedFps, deltaTime, fpsToDisplay) = Main.updateFps fps timestamp

    let rotation = GameState.rotation state
    let updatedRotation = rotation + (0.0001 * deltaTime)

    Main.render (GameState.canvas state) fpsToDisplay updatedRotation

    let entities = GameState.entities state
    let updatedEntities = map (\entity -> Entity.update entity deltaTime) entities
    Canvas.render (GameState.canvas state) $ mapM Entity.render updatedEntities

    let newState = state { GameState.fps = updatedFps, GameState.rotation = updatedRotation, GameState.entities = updatedEntities }

    AnimationFrame.requestAnimationFrame $ Main.mainLoop newState
    return ()

updateFps :: Fps -> Double -> (Fps, Double, Integer)
updateFps fps timestamp =
    let
        previousTimestamp = Fps.timestamp fps
        deltaTime = timestamp - previousTimestamp
        candidateTimeCount = Fps.timeCount fps + deltaTime
        candidateFrameCount = Fps.frameCount fps + 1
        (updatedTimeCount, updatedFrameCount, fpsToDisplay)
            | candidateTimeCount < 1000 = (candidateTimeCount, candidateFrameCount, -1)
            | otherwise                 = (candidateTimeCount - 1000, 1, candidateFrameCount)
        updatedFps = Fps { Fps.timeCount = updatedTimeCount, Fps.frameCount = updatedFrameCount, Fps.timestamp = timestamp }
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