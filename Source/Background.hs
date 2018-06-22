{-# LANGUAGE NamedFieldPuns #-}

module Background
    ( Background()
    , Background.new
    ) where

    import Entity
    import Sprite
    import Constants
    import Resources
    import Point

    data Background = Background

    new :: Background
    new = Background

    instance EntityClass Background where
        load background = imageDefs background
        render background input = Sprite.render background input

    instance Sprite Background where
        imageDef _ = (ResourceKey "Background", Image, "Resources/Background.jpg")
        position _ = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        setPosition background _ = background
        rotation _ = 0