{-# LANGUAGE NamedFieldPuns #-}

module Collidable
    ( Collidable
    , Collidable.haveCollided
    , Collidable.render
    , Collidable.renderAtPosition
    ) where

    import Sprite (Sprite, boundingBox, width, height, position, renderAtPosition, bitmapData, rotation, position, dimensions)
    import Rectangle (left, right, top, bottom)
    import Resources (Resources, BitmapData(BitmapData, _collisionPolygon))
    import Point (Point(Point, x, y), normal, dot)
    import Haste.Graphics.Canvas as Canvas (Picture, circle, stroke, color, Color(RGB), line, Shape, path, rotate, translate)
    import CollisionPolygon (CollisionPolygon(CollisionPolygon, points))

    debugColor :: Color
    debugColor = RGB 0 255 0

    strokeDebug :: Canvas.Shape () -> Canvas.Picture ()
    strokeDebug shape = Canvas.color debugColor $ Canvas.stroke shape

    normals :: [Point] -> [Point]
    normals [] = []
    normals [a] = []
    normals (a : b : rest) = Point.normal a b : normals (b : rest)

    project :: [Point] -> Point -> (Double, Double)
    project vertices axis =
        let
            projectedVertices = map (\vertex -> Point.dot vertex axis) vertices
            minVertex = minimum projectedVertices
            maxVertex = maximum projectedVertices
        in
            (minVertex, maxVertex)

    overlap :: (Double, Double) -> (Double, Double) -> Bool
    overlap (fromA, toA) (fromB, toB) = toA >= fromB && toB >= fromA

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

        transformPoint :: a -> Resources -> Point -> Point
        transformPoint a resources Point{x = originalX, y = originalY} =
            let
                Point{x = ax, y = ay} = Sprite.position a
                rotation = Sprite.rotation a
                (width, height) = Sprite.dimensions a resources

                pointX = -(width / 2) + originalX
                pointY = -(height / 2) + originalY
                rotatedPointX = (pointX * (cos rotation)) - (pointY * (sin rotation))
                rotatedPointY = (pointX * (sin rotation)) + (pointY * (cos rotation))
                translatedPointX = rotatedPointX + ax
                translatedPointY = rotatedPointY + ay
            in
                Point { x = translatedPointX, y = translatedPointY }

        transformedCollisionPolygonPoints :: a -> Resources -> [Point]
        transformedCollisionPolygonPoints a resources =
            let
                CollisionPolygon{points} = collisionPolygon a resources
            in
                map (\point -> transformPoint a resources point) points

        haveCollidedPolygons :: Collidable b => a -> b -> Resources -> Bool
        haveCollidedPolygons a b resources =
            let
                pointsA = transformedCollisionPolygonPoints a resources
                pointsB = transformedCollisionPolygonPoints b resources

                normalsA = normals pointsA
                normalsB = normals pointsB

                axes = normalsA ++ normalsB
            in
                all (\axis -> overlap (project pointsA axis) (project pointsB axis)) axes

        haveCollided :: Collidable b => a -> b -> Resources -> Bool
        haveCollided a b resources = (haveCollidedCircle a b resources) && (haveCollidedPolygons a b resources)

        render :: a -> Resources -> Canvas.Picture ()
        render a resources = Collidable.renderAtPosition a resources $ position a

        renderAtPosition :: a -> Resources -> Point -> Canvas.Picture ()
        renderAtPosition a resources point@Point{x, y} = do 
            Sprite.renderAtPosition a resources point
            renderCollisionCrosshair a
            renderCollisionCircle a resources
            renderCollisionPolygon a resources

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

        renderCollisionPolygon :: a -> Resources -> Canvas.Picture ()
        renderCollisionPolygon a resources =
            let
                transformedPoints = transformedCollisionPolygonPoints a resources
                transformedPoints' = map (\Point{x = polygonX, y = polygonY} -> (polygonX, polygonY)) transformedPoints
            in
                strokeDebug $ Canvas.path transformedPoints'

        collisionPolygon :: a -> Resources -> CollisionPolygon
        collisionPolygon a resources = 
            let
                BitmapData{_collisionPolygon} = Sprite.bitmapData a resources
            in
                _collisionPolygon