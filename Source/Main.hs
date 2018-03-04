module Main where

import Haste.DOM as DOM
import Other

main :: IO ()
main = do
    let number = Other.calculate 12
    textElement <- DOM.newTextElem $ "Hello World!!! " ++ show number
    DOM.appendChild DOM.documentBody textElement