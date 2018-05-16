{-# LANGUAGE NamedFieldPuns #-}

module Sprite where

    import Resources (Resources(Resources, images), ResourceDef, BitmapData(BitmapData, _bitmap, _width, _height))
    import Point (Point(Point, x, y))
    import Data.Map ((!))
    import Rectangle (Rectangle(Rectangle, topLeft, bottomRight))
    import Haste.Graphics.Canvas as Canvas (Picture, Bitmap, draw, rotate, translate)
    import Input(Input)

    class Sprite a where

        imageDef :: a -> Resources.ResourceDef

        position :: a -> Point

        rotation :: a -> Double

        bitmapData :: a -> Resources -> Resources.BitmapData
        bitmapData a Resources{images} = images ! (fst $ imageDef a)

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

        render :: a -> Resources -> Canvas.Picture ()
        render = defaultRender

        defaultRender :: a -> Resources -> Canvas.Picture ()
        defaultRender a resources = renderAtPosition a resources $ position a

        renderAtPosition :: a -> Resources -> Point -> Canvas.Picture ()
        renderAtPosition a resources Point{x, y} =
            let
                BitmapData{_bitmap, _width, _height} = bitmapData a resources
                drawnSprite = Canvas.draw _bitmap (-(_width / 2), -(_height / 2))
                rotatedSprite = Canvas.rotate (rotation a) drawnSprite
                translatedSprite = Canvas.translate (x, y) rotatedSprite
            in
                translatedSprite