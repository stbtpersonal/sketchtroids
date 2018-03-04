module Main where

import Haste.DOM as Dom

main :: IO ()
main = do
    textElement <- Dom.newTextElem "Hello World!!!"
    Dom.appendChild Dom.documentBody textElement