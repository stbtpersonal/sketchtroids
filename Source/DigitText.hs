{-# LANGUAGE NamedFieldPuns #-}

module DigitText
    ( DigitText()
    , DigitText.new
    ) where

    import Point
    import Entity
    import Sprite
    import Resources

    data DigitText = DigitText
        { _position :: Point
        , _digit :: Int
        }

    new :: Point -> Int -> DigitText
    new position digit = DigitText
        { _position = position
        , _digit = digit
        }

    instance EntityClass DigitText where
        load digitText = imageDefs digitText
        update digitText input = Entity $ Sprite.update digitText input
        render digitText input = Sprite.render digitText input

    instance Sprite DigitText where
        imageDefs _ =
            [ (ResourceKey "0Text", Image, "Resources/0Text.png")
            , (ResourceKey "1Text", Image, "Resources/1Text.png")
            , (ResourceKey "2Text", Image, "Resources/2Text.png")
            , (ResourceKey "3Text", Image, "Resources/3Text.png")
            , (ResourceKey "4Text", Image, "Resources/4Text.png")
            , (ResourceKey "5Text", Image, "Resources/5Text.png")
            , (ResourceKey "6Text", Image, "Resources/6Text.png")
            , (ResourceKey "7Text", Image, "Resources/7Text.png")
            , (ResourceKey "8Text", Image, "Resources/8Text.png")
            , (ResourceKey "9Text", Image, "Resources/9Text.png")
            ]
        position DigitText{_position} = _position
        setPosition digitText position = digitText{_position = position}
        rotation _ = 0
        spriteIndex DigitText{_digit} = _digit
