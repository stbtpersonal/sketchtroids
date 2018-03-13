{-# LANGUAGE RecordWildCards #-}

module Modes.GameMode.Entities.Background where

    import Entity
    import Haste.Graphics.Canvas as Canvas
    import Constants

    data Background = Background { width :: Double, height :: Double }

    new :: Background
    new = Background { width = Constants.nativeWidth, height = Constants.nativeHeight }

    instance EntityClass Background where

        render background@Background{..} = 
            let
                shape = Canvas.rect (0, 0) (width, height)
                filled = Canvas.fill shape
                colored = Canvas.color (Canvas.RGB 255 255 255) filled
            in
                colored