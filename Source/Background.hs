{-# LANGUAGE NamedFieldPuns #-}

module Background
    ( Background()
    , Background.new
    ) where

    import Entity (EntityClass(render))
    import Haste.Graphics.Canvas as Canvas (rect, fill, color, Color(RGB))
    import Constants (nativeWidth, nativeHeight)

    data Background = Background
        { _width :: Double
        , _height :: Double
        }

    new :: Background
    new = Background
        { _width = Constants.nativeWidth
        , _height = Constants.nativeHeight
        }

    instance EntityClass Background where

        render Background{_width, _height} _ = 
            let
                shape = Canvas.rect (0, 0) (_width, _height)
                filled = Canvas.fill shape
                colored = Canvas.color (Canvas.RGB 255 255 255) filled
            in
                colored