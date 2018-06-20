{-# LANGUAGE NamedFieldPuns #-}

module Score
    ( Score()
    , Score.new
    , Score.setScore
    ) where

    import NumberText
    import Sprite
    import Entity
    import Point

    data Score = Score
        { _numberText :: NumberText
        }

    new :: Score
    new = Score
        { _numberText = NumberText.new Point { x = 1500, y = 500 }
        }

    setScore :: Score -> Integer -> Score
    setScore score@Score{_numberText} number = score{_numberText = NumberText.setNumber _numberText number}

    instance EntityClass Score where
        load Score{_numberText} = Entity.load _numberText
        render Score{_numberText} input = Entity.render _numberText input