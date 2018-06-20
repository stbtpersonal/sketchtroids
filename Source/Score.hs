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

    data Score = Score
        { _numberText :: NumberText
        }

    new :: Score
    new = Score
        { _numberText = NumberText.new Point { x = (Point.x screenPosition) + 175, y = Point.y screenPosition }
        }

    setScore :: Score -> Integer -> Score
    setScore score@Score{_numberText} number = score{_numberText = NumberText.setNumber _numberText number}

    addScore :: Score -> Integer -> Score
    addScore score@Score{_numberText} number = setScore score (number + NumberText.getNumber _numberText)

    screenPosition :: Point
    screenPosition = Point { x = 175, y = Constants.nativeHeight - 75 }

    instance EntityClass Score where
        load score@Score{_numberText} = Entity.load _numberText ++ Sprite.imageDefs score
        render score@Score{_numberText} input = do
            Entity.render _numberText input
            Sprite.render score input

    instance Sprite Score where
        imageDef _ = (ResourceKey "ScoreText", "Resources/ScoreText.png")
        position _ = screenPosition
        setPosition score _ = score
        rotation _ = 0