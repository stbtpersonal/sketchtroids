{-# LANGUAGE NamedFieldPuns #-}

module Collidable where

    import Sprite
    import Rectangle
    import Resources
    import Point

    class Sprite a => Collidable a where

        haveCollidedAabb :: Collidable b => a -> b -> Resources -> Bool
        haveCollidedAabb a b resources = 
            let
                boundingBoxA = boundingBox a resources
                boundingBoxB = boundingBox b resources
            in
                (Rectangle.right boundingBoxA >= Rectangle.left boundingBoxB) &&
                (Rectangle.left boundingBoxA <= Rectangle.right boundingBoxB) &&
                (Rectangle.bottom boundingBoxA >= Rectangle.top boundingBoxB) &&
                (Rectangle.top boundingBoxA <= Rectangle.bottom boundingBoxB)

        haveCollidedCircle :: Collidable b => a -> b -> Resources -> Bool
        haveCollidedCircle a b resources =
            let
                radius c = sqrt ((Sprite.width c resources ** 2) + (Sprite.height c resources ** 2)) / 2
                radiusA = radius a
                radiusB = radius b

                positionA = Sprite.position' a
                positionB = Sprite.position' b
                dx = (Point.x positionB) - (Point.x positionA)
                dy = (Point.y positionB) - (Point.y positionA)
                distance = sqrt ((dx ** 2) + (dy ** 2))
            in
                distance < radiusA + radiusB

        haveCollided :: Collidable b => a -> b -> Resources -> Bool
        haveCollided = haveCollidedCircle