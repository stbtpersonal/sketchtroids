module Renderer (initialize, Renderer.render) where

    import Haste
    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas
    import Constants

    initialize :: IO Canvas
    initialize = do
        canvas <- Renderer.createCanvas
        Just contentElement <- DOM.elemById "content"
        DOM.appendChild contentElement canvas
        return canvas

    createCanvas :: IO Canvas
    createCanvas = do
        canvasElement <- DOM.newElem "canvas"
        DOM.setAttr canvasElement "id" "canvas"
        DOM.setAttr canvasElement "width" $ show Constants.nativeWidth
        DOM.setAttr canvasElement "height" $ show Constants.nativeHeight
        Just canvas <- DOM.fromElem canvasElement
        return canvas

    render :: Haste.MonadIO m => Canvas -> Picture a -> m a
    render = Canvas.render