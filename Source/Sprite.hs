{-# LANGUAGE NamedFieldPuns #-}

module Sprite
    ( Sprite.Sprite
    , Sprite.imageDef
    , Sprite.imageDefs
    , Sprite.position
    , Sprite.setPosition
    , Sprite.rotation
    , Sprite.isEnabled
    , Sprite.setEnabled
    , Sprite.isWrappingHorizontal
    , Sprite.isWrappingVertical
    , Sprite.spriteIndex
    , Sprite.setSpriteIndex
    , Sprite.bitmapData
    , Sprite.bitmap
    , Sprite.dimensions
    , Sprite.width
    , Sprite.height
    , Sprite.boundingBox
    , Sprite.update
    , Sprite.render
    , Sprite.defaultRender
    , Sprite.getRenderSprites
    ) where

    import Resources
    import Point
    import Data.Map
    import Rectangle
    import Haste.Graphics.Canvas as Canvas hiding (Point)
    import Input
    import Renderer
    import Constants

    class Sprite a where

        imageDef :: a -> Resources.ResourceDef
        imageDef a = (imageDefs a) !! (spriteIndex a)

        imageDefs :: a -> [Resources.ResourceDef]
        imageDefs a = [imageDef a]

        position :: a -> Point

        setPosition :: a -> Point -> a

        rotation :: a -> Double

        isEnabled :: a -> Bool
        isEnabled _ = True

        setEnabled :: a -> Bool -> a
        setEnabled a _ = a

        isWrappingHorizontal :: a -> Bool
        isWrappingHorizontal _ = False

        isWrappingVertical :: a -> Bool
        isWrappingVertical _ = False

        spriteIndex :: a -> Int
        spriteIndex _ = 0

        setSpriteIndex :: a -> Int -> a
        setSpriteIndex a _ = a

        bitmapData :: a -> Resources -> Resources.BitmapData
        bitmapData a Resources{images} = 
            let
                (key, _, _) = imageDef a
            in
                images ! key

        bitmap :: a -> Resources -> Canvas.Bitmap
        bitmap a resources = 
            let
                BitmapData{_bitmap} = bitmapData a resources
            in 
                _bitmap

        dimensions :: a -> Resources -> (Double, Double)
        dimensions a resources = 
            let
                BitmapData{_width, _height} = bitmapData a resources
            in
                (_width, _height)

        width :: a -> Resources -> Double
        width a resources = fst $ dimensions a resources

        height :: a -> Resources -> Double
        height a resources = snd $ dimensions a resources

        boundingBox :: a -> Resources -> Rectangle
        boundingBox a resources = 
            let
                (width, height) = dimensions a resources
                Point{x, y} = position a
            in
                Rectangle
                    { topLeft = Point { x = x - (width / 2), y = y - (height / 2) }
                    , bottomRight = Point { x = x + (width / 2), y = y + (width / 2) }
                    }

        update :: a -> Input -> a
        update a _ = a

        render :: a -> Input -> Canvas.Picture ()
        render = defaultRender

        defaultRender :: a -> Input -> Canvas.Picture ()
        defaultRender a Input{resources} = if isEnabled a
            then
                let
                    renderSprites = getRenderSprites a resources
                in
                    mapM_ (\renderSprite -> renderAtPosition renderSprite resources) renderSprites
            else
                Renderer.doNothing

        getRenderSprites :: a -> Resources -> [a]
        getRenderSprites a resources =
            let
                notWrapped = [a]

                (width, height) = dimensions a resources
                Point{x, y} = position a

                wrappedLeft = if isWrappingHorizontal a && x < width / 2
                    then [setPosition a Point { x = Constants.nativeWidth + x, y = y }]
                    else []

                wrappedRight = if isWrappingHorizontal a && x > Constants.nativeWidth - (width / 2)
                    then [setPosition a Point { x = x - Constants.nativeWidth, y = y }]
                    else []

                wrappedTop = if isWrappingVertical a && y < height / 2
                    then [setPosition a Point { x = x, y = Constants.nativeHeight + y }]
                    else []

                wrappedBottom = if isWrappingVertical a && y > Constants.nativeHeight - (height / 2)
                    then [setPosition a Point { x = x, y = y - Constants.nativeHeight}]
                    else []
            in
                notWrapped ++ wrappedLeft ++ wrappedRight ++ wrappedTop ++ wrappedBottom

        renderAtPosition :: a -> Resources -> Canvas.Picture ()
        renderAtPosition a resources = 
            let
                Point{x, y} = position a
                BitmapData{_bitmap, _width, _height} = bitmapData a resources
                drawnSprite = Canvas.draw _bitmap (-(_width / 2), -(_height / 2))
                rotatedSprite = Canvas.rotate (rotation a) drawnSprite
                translatedSprite = Canvas.translate (x, y) rotatedSprite
            in
                translatedSprite