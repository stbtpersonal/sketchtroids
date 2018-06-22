{-# LANGUAGE NamedFieldPuns #-}

module Score
    ( Score()
    , Score.new
    , Score.setScore
    , Score.addScore
    ) where

    import NumberText
    import Sprite
    import Entity
    import Point
    import Resources
    import Constants
    import Renderer

    data Score = Score
        { _numberText :: NumberText
        , _isEnabled :: Bool
        }

    new :: Score
    new = Score
        { _numberText = NumberText.new Point { x = (Point.x screenPosition) + 225, y = Point.y screenPosition }
        , _isEnabled = True
        }

    setScore :: Score -> Integer -> Score
    setScore score@Score{_numberText} number = score{_numberText = NumberText.setNumber _numberText number}

    addScore :: Score -> Integer -> Score
    addScore score@Score{_numberText} number = setScore score (number + NumberText.getNumber _numberText)

    screenPosition :: Point
    screenPosition = Point { x = 175, y = Constants.nativeHeight - 75 }

    instance EntityClass Score where
        load score@Score{_numberText} = Entity.load _numberText ++ Sprite.imageDefs score
        render score@Score{_numberText, _isEnabled} input = if _isEnabled
            then
                do
                    Entity.render _numberText input
                    Sprite.render score input
            else
                Renderer.doNothing

    instance Sprite Score where
        imageDef _ = (ResourceKey "ScoreText", Image, "Resources/ScoreText.png")
        position _ = screenPosition
        setPosition score _ = score
        rotation _ = 0
        isEnabled Score{_isEnabled} = _isEnabled
        setEnabled score enabled = score{_isEnabled = enabled}