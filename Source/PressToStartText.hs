{-# LANGUAGE NamedFieldPuns #-}

module PressToStartText
    ( PressToStartText(PressToStartText)
    , PressToStartText.new
    ) where

    import Entity (EntityClass(load, render, update), Entity(Entity))
    import Sprite (Sprite(imageDef, position, setPosition, rotation), render, defaultRender, update, isEnabled, setEnabled)
    import Resources (ResourceKey(ResourceKey))
    import Point (Point(Point, x, y))
    import Constants (nativeWidth, nativeHeight)
    import Input (Input(Input, deltaTime, keyboard))
    import Haste.Graphics.Canvas as Canvas (opacity)
    import Keyboard (action)

    data PressToStartText = PressToStartText
        { _alpha :: Double
        , _fadeDirection :: Double
        , _isStopping :: Bool
        , _isStopped :: Bool
        , _isEnabled :: Bool
        }

    new :: PressToStartText
    new = PressToStartText
        { _alpha = 0
        , _fadeDirection = 1
        , _isStopping = False
        , _isStopped = False
        , _isEnabled = True
        }

    fadeDuration :: Double
    fadeDuration = 1000

    instance EntityClass PressToStartText where
        load text = [imageDef text]
        render = Sprite.render
        update text input = Entity $ Sprite.update text input

    instance Sprite PressToStartText where
        imageDef _ = (ResourceKey "PressToStartText", "Resources/PressToStartText.png")
        position _ = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        setPosition text _ = text
        rotation _ = 0

        update text@PressToStartText{_alpha, _fadeDirection, _isStopping, _isStopped} Input{deltaTime, keyboard} = if Sprite.isEnabled text
            then
                let
                    isEnabled' = not _isStopped

                    fadeMultiplier = 1 / fadeDuration
                    fadeDelta = _fadeDirection * (fadeMultiplier * deltaTime)
                    alpha' = _alpha + fadeDelta
                    fadeDirection' = if alpha' >= 0 && alpha' <= 1 then _fadeDirection else -1 * _fadeDirection
                    alpha''
                        | _isStopped = 0
                        | alpha' < 0 = -1 * alpha'
                        | alpha' > 1 = 2 - alpha'
                        | otherwise  = alpha'   

                    isStopping' = _isStopping || Keyboard.action keyboard
                    isStopped' = _isStopped || (isStopping' && alpha' <= 0)
                in
                    text
                        { _alpha = alpha''
                        , _fadeDirection = fadeDirection'
                        , _isStopping = isStopping'
                        , _isStopped = isStopped'
                        , _isEnabled = isEnabled'
                        }
            else
                text{_isEnabled = False}

        render text@PressToStartText{_alpha} input = Canvas.opacity _alpha $ Sprite.defaultRender text input

        isEnabled PressToStartText{_isEnabled} = _isEnabled

        setEnabled text enabled = text{_isEnabled = enabled}