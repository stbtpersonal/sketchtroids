{-# LANGUAGE NamedFieldPuns #-}

module Collidable
    ( Collidable
    , Collidable.haveCollided
    , Collidable.render
    , Collidable.renderAtPosition
    ) where

    import Sprite (Sprite, boundingBox, width, height, position, renderAtPosition)
    import Rectangle (left, right, top, bottom)
    import Resources (Resources)
    import Point (Point(Point, x, y))
    import Haste.Graphics.Canvas as Canvas (Picture, circle, stroke, color, Color(RGB), line, Shape)

    debugColor :: Color
    debugColor = RGB 0 255 0

    strokeDebug :: Canvas.Shape () -> Canvas.Picture ()
    strokeDebug shape = Canvas.color debugColor $ Canvas.stroke shape

    class Sprite a => Collidable a where

        radius :: a -> Resources -> Double
        radius a resources = sqrt ((Sprite.width a resources ** 2) + (Sprite.height a resources ** 2)) / 2

        haveCollidedCircle :: Collidable b => a -> b -> Resources -> Bool
        haveCollidedCircle a b resources =
            let
                radiusA = radius a resources
                radiusB = radius b resources

                positionA = Sprite.position a
                positionB = Sprite.position b
                dx = (Point.x positionB) - (Point.x positionA)
                dy = (Point.y positionB) - (Point.y positionA)
                distance = sqrt ((dx ** 2) + (dy ** 2))
            in
                distance < radiusA + radiusB

        haveCollided :: Collidable b => a -> b -> Resources -> Bool
        haveCollided = haveCollidedCircle

        render :: a -> Resources -> Canvas.Picture ()
        render a resources = Collidable.renderAtPosition a resources $ position a

        renderAtPosition :: a -> Resources -> Point -> Canvas.Picture ()
        renderAtPosition a resources point@Point{x, y} = do 
            Sprite.renderAtPosition a resources point
            renderCollisionCrosshair a
            renderCollisionCircle a resources

        renderCollisionCrosshair :: a -> Canvas.Picture()
        renderCollisionCrosshair a = 
            let
                Point{x, y} = Sprite.position a
            in
                do
                    strokeDebug $ Canvas.line (x - 10, y) (x + 10, y)
                    strokeDebug $ Canvas.line (x, y - 10) (x, y + 10)

        renderCollisionCircle :: a -> Resources -> Canvas.Picture ()
        renderCollisionCircle a resources =
            let
                Point{x, y} = Sprite.position a
                radius' = radius a resources
            in
                strokeDebug $ Canvas.circle (x, y) radius'