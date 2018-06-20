{-# LANGUAGE NamedFieldPuns #-}

module NumberText
    ( NumberText()
    , NumberText.new
    , NumberText.setNumber
    , NumberText.getNumber
    ) where

    import Point
    import Sprite
    import Resources
    import DigitText
    import Entity

    data NumberText = NumberText
        { _position :: Point
        , _number :: Integer
        , _digitTexts :: [DigitText]
        }

    new :: Point -> NumberText
    new position = NumberText
        { _position = position
        , _number = 0
        , _digitTexts = [zeroDigit]
        }

    zeroDigit :: DigitText
    zeroDigit = DigitText.new Point.zero 0

    setNumber :: NumberText -> Integer -> NumberText
    setNumber numberText@NumberText{_position} number = numberText{_number = number, _digitTexts = buildDigitTexts _position number}

    getNumber :: NumberText -> Integer
    getNumber NumberText{_number} = _number

    digitOffset :: Double
    digitOffset = 50

    buildDigitTexts :: Point -> Integer -> [DigitText]
    buildDigitTexts basePosition number =
        let
            numberCharacters = show number
            numberStrings = map (\character -> [character]) numberCharacters
            digits = map read numberStrings
            digitsAndIndeces = (zip digits [0..])
            
            Point{x = baseX} = basePosition
            getOffset index = basePosition{x = baseX + (fromIntegral index) * digitOffset}

        in
            map (\(digit, index) -> DigitText.new (getOffset index) digit) digitsAndIndeces

    instance EntityClass NumberText where
        load _ = Sprite.imageDefs zeroDigit
        render NumberText{_digitTexts} input = Entity.renderAll (map Entity _digitTexts) input