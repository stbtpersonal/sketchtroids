{-# LANGUAGE NamedFieldPuns #-}

module PleaseWaitText
    ( PleaseWaitText()
    , PleaseWaitText.new
    ) where
    
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Renderer
    import Constants

    data PleaseWaitText = PleaseWaitText

    new :: PleaseWaitText
    new = PleaseWaitText

    waitText = "Hang on..."
    waitFont = "50px sans-serif"

    instance EntityClass PleaseWaitText where
        render _ _ = do
            (width, height) <- Renderer.measureText waitText waitFont

            let text = Canvas.text (-width / 2, height / 2) waitText
            let fonted = Canvas.font waitFont text
            let colored = Canvas.color (Canvas.RGB 255 255 255) fonted
            let translated = Canvas.translate (Constants.nativeWidth / 2, Constants.nativeHeight / 2) colored

            translated