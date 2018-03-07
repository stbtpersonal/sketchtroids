module Renderer (initialize, Renderer.render) where

    import Haste
    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas

    initialize :: Double -> Double -> IO Canvas
    initialize width height = do
        canvas <- Renderer.createCanvas width height
        Just contentElement <- DOM.elemById "content"
        DOM.appendChild contentElement canvas
        return canvas

    createCanvas :: Double -> Double -> IO Canvas
    createCanvas width height = do
        canvasElement <- DOM.newElem "canvas"
        DOM.setAttr canvasElement "id" "canvas"
        DOM.setAttr canvasElement "width" $ show width
        DOM.setAttr canvasElement "height" $ show height
        Just canvas <- DOM.fromElem canvasElement
        return canvas

    render :: Haste.MonadIO m => Canvas -> Picture a -> m a
    render = Canvas.render