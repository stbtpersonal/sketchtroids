module Main where

import Haste.DOM as DOM
import Other
import Haste.Graphics.AnimationFrame as AnimationFrame

main :: IO ()
main = do
    divElement <- DOM.newElem "div"
    DOM.setAttr divElement "id" "text"
    DOM.appendChild DOM.documentBody divElement

    AnimationFrame.requestAnimationFrame mainLoop
    return ()


mainLoop :: AnimationFrame.HRTimeStamp -> IO ()
mainLoop timestamp = do
    Just divElement <- DOM.elemById "text"
    DOM.clearChildren divElement
    let number = Other.calculate $ round timestamp
    textElement <- DOM.newTextElem $ "Hello World!!! " ++ show number
    DOM.appendChild divElement textElement
    AnimationFrame.requestAnimationFrame mainLoop
    return ()