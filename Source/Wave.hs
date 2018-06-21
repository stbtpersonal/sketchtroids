{-# LANGUAGE NamedFieldPuns #-}

module Wave
    ( Wave()
    , Wave.new
    , Wave.setWave
    , Wave.incrementWave
    ) where

    import NumberText
    import Sprite
    import Entity
    import Point
    import Resources
    import Constants
    import Renderer

    data Wave = Wave
        { _numberText :: NumberText
        , _isEnabled :: Bool
        }

    new :: Wave
    new = Wave
        { _numberText = NumberText.new Point { x = (Point.x screenPosition) + 175, y = Point.y screenPosition }
        , _isEnabled = True
        }

    setWave :: Wave -> Integer -> Wave
    setWave wave@Wave{_numberText} number = wave{_numberText = NumberText.setNumber _numberText number}

    incrementWave :: Wave -> Wave
    incrementWave wave@Wave{_numberText} = setWave wave (1 + NumberText.getNumber _numberText)

    screenPosition :: Point
    screenPosition = Point { x = 175, y = Constants.nativeHeight - 175 }

    instance EntityClass Wave where
        load wave@Wave{_numberText} = Entity.load _numberText ++ Sprite.imageDefs wave
        render wave@Wave{_numberText, _isEnabled} input = if _isEnabled
            then
                do
                    Entity.render _numberText input
                    Sprite.render wave input
            else
                Renderer.doNothing

    instance Sprite Wave where
        imageDef _ = (ResourceKey "WaveText", "Resources/WaveText.png")
        position _ = screenPosition
        setPosition wave _ = wave
        rotation _ = 0
        isEnabled Wave{_isEnabled} = _isEnabled
        setEnabled wave enabled = wave{_isEnabled = enabled}