{-# LANGUAGE NamedFieldPuns #-}

module PressToStartText
    ( PressToStartText(PressToStartText)
    , PressToStartText.new
    ) where

    import Entity (EntityClass(load, render, update), Entity(Entity))
    import Sprite (Sprite(imageDef, position, rotation), render, defaultRender, update)
    import Resources (ResourceKey(ResourceKey))
    import Point (Point(Point, x, y))
    import Constants (nativeWidth, nativeHeight)
    import Input (Input(Input, deltaTime))
    import Haste.Graphics.Canvas as Canvas (opacity)

    data PressToStartText = PressToStartText { _alpha :: Double, _fadeDirection :: Double }

    new :: PressToStartText
    new = PressToStartText { _alpha = 0, _fadeDirection = 1 }

    fadeDuration :: Double
    fadeDuration = 1000

    instance EntityClass PressToStartText where
        load text = [imageDef text]
        render = Sprite.render
        update text input = Entity $ Sprite.update text input

    instance Sprite PressToStartText where
        imageDef _ = (ResourceKey "PressToStartText", "Resources/PressToStartText.png")
        position _ = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        rotation _ = 0

        update text@PressToStartText{_alpha, _fadeDirection} Input{deltaTime} =
            let
                fadeMultiplier = 1 / fadeDuration
                fadeDelta = _fadeDirection * (fadeMultiplier * deltaTime)
                alpha' = _alpha + fadeDelta
                fadeDirection' = if alpha' >= 0 && alpha' <= 1 then _fadeDirection else -1 * _fadeDirection
                alpha''
                    | alpha' < 0 = -1 * alpha'
                    | alpha' > 1 = 2 - alpha'
                    | otherwise  = alpha'
            in
                text
                    { _alpha = alpha''
                    , _fadeDirection = fadeDirection'
                    }

        render text@PressToStartText{_alpha} resorces = Canvas.opacity _alpha $ Sprite.defaultRender text resorces