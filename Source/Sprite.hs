{-# LANGUAGE NamedFieldPuns #-}

module Sprite where

    import Resources
    import Point
    import Data.Map as Map
    import Rectangle
    import Haste.Graphics.Canvas as Canvas

    class Sprite a where

        imageDef' :: a -> Resources.ResourceDef

        position' :: a -> Point.Point

        rotation' :: a -> Double

        bitmapData :: a -> Resources -> Resources.BitmapData
        bitmapData a Resources{images} = images ! (fst $ imageDef' a)

        bitmap :: a -> Resources -> Canvas.Bitmap
        bitmap a resources = fst $ bitmapData a resources

        dimensions :: a -> Resources -> (Double, Double)
        dimensions a resources = snd $ bitmapData a resources

        width :: a -> Resources -> Double
        width a resources = fst $ dimensions a resources

        height :: a -> Resources -> Double
        height a resources = snd $ dimensions a resources

        boundingBox :: a -> Resources -> Rectangle
        boundingBox a resources = 
            let
                (width, height) = dimensions a resources
                x = Point.x $ position' a
                y = Point.y $ position' a
            in
                Rectangle
                    { topLeft = Point.Point { x = x - (width / 2), y = y - (height / 2) }
                    , bottomRight = Point.Point { x = x + (width / 2), y = y + (width / 2) }
                    }

        drawAtPosition :: a -> Resources -> Point.Point -> Canvas.Picture ()
        drawAtPosition a resources Point{x, y} =
            let
                rotation = rotation' a
                (bitmap, (width, height)) = bitmapData a resources
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                translatedSprite = Canvas.translate (x, y) rotatedSprite
            in
                translatedSprite