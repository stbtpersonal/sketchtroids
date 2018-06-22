module Renderer
    ( Renderer.initialize
    , Renderer.render
    , Renderer.doNothing
    , Renderer.resize
    , Renderer.measureText
    ) where

    import Haste
    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas
    import Constants
    import Control.Monad
    import Haste.Foreign as Foreign
    import Utils

    initialize :: IO Canvas
    initialize = do
        canvas <- Renderer.createCanvas
        Just contentElement <- DOM.elemById "content"
        DOM.appendChild contentElement canvas
        return canvas

    createCanvas :: IO Canvas
    createCanvas = do
        canvasElement <- DOM.newElem "canvas"
        DOM.setProp canvasElement "id" "canvas"
        DOM.setProp canvasElement "width" $ show Constants.nativeWidth
        DOM.setProp canvasElement "height" $ show Constants.nativeHeight
        Just canvas <- DOM.fromElem canvasElement
        return canvas

    render :: Haste.MonadIO m => Canvas -> Picture a -> m a
    render = Canvas.render

    doNothing :: Canvas.Picture ()
    doNothing = Canvas.setStrokeColor $ Canvas.RGB 0 0 0

    resize :: Canvas -> IO Double
    resize canvas = do
        Just contentElement <- DOM.elemById "content"
        viewportWidthProperty <- DOM.getProp contentElement "clientWidth"
        viewportHeightProperty <- DOM.getProp contentElement "clientHeight"
        let viewportWidth = read viewportWidthProperty
        let viewportHeight = read viewportHeightProperty

        let widthScale = viewportWidth / Constants.nativeWidth
        let heightScale = viewportHeight / Constants.nativeHeight
        let smallestScale = min widthScale heightScale

        let resolvedWidth = round $ Constants.nativeWidth * smallestScale
        let resolvedHeight = round $ Constants.nativeHeight * smallestScale
        
        let canvasElement = DOM.elemOf canvas
        currentWidthProperty <- DOM.getProp canvasElement "width"
        currentHeightProperty <- DOM.getProp canvasElement "height"
        let currentWidth = read currentWidthProperty
        let currentHeight = read currentHeightProperty

        when (currentWidth /= resolvedWidth || currentHeight /= resolvedHeight) $ do
            DOM.setProp canvasElement "width" $ show resolvedWidth
            DOM.setProp canvasElement "height" $ show resolvedHeight

        return smallestScale

    measureText :: String -> String -> Canvas.Picture (Double, Double)
    measureText text font = Canvas.withContext $ \context -> do
        width <- jsMeasureTextWidth context text font
        height <- jsMeasureTextWidth context "M" font
        return (width, height)

    jsMeasureTextWidth :: Canvas.Ctx -> String -> String -> IO Double
    jsMeasureTextWidth = Foreign.ffi $ Haste.toJSString "RENDERER['measureTextWidth']"