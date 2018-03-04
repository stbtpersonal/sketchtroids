module Main where

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
    Just divElement <- DOM.elemById "text"

    DOM.clearChildren divElement

    let counter = GameState.getCounter state
    textElement <- DOM.newTextElem $ "Hello World!!! " ++ show counter
    DOM.appendChild divElement textElement

    let newState = GameState.setCounter state $ counter + 150

    AnimationFrame.requestAnimationFrame $ mainLoop newState
    return ()