module Entities.Background where

    import Entity
    import Haste.Graphics.Canvas as Canvas

    data Background = Background { width :: Double, height :: Double }

    new :: Double -> Double -> Background
    new width height = Background { width = width, height = height }

    instance EntityClass Background where

        update background input = Entity $ background

        render background = 
            let
                width = Entities.Background.width background
                height = Entities.Background.height background

                shape = Canvas.rect (0, 0) (width, height)
                filled = Canvas.fill shape
                colored = Canvas.color (Canvas.RGB 255 255 255) filled
            in
                colored