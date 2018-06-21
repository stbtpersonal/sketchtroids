{-# LANGUAGE NamedFieldPuns #-}

module Collidable
    ( Collidable
    , Collidable.haveCollided
    , Collidable.render
    , Collidable.defaultRender
    ) where

    import Sprite
    import Rectangle
    import Resources
    import Point
    import Haste.Graphics.Canvas as Canvas hiding (Point)
    import CollisionPolygon
    import Renderer
    import Input
    import Control.Monad

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
        haveCollided a b resources = 
            let
                checkSprite a' b' = 
                    (Sprite.isEnabled a') &&
                    (Sprite.isEnabled b') &&
                    (haveCollidedCircle a' b' resources) &&
                    (haveCollidedPolygons a' b' resources)
                renderSpritesA = Sprite.getRenderSprites a resources
                renderSpritesB = Sprite.getRenderSprites b resources
            in
                or $ concatMap (\renderSpriteA -> map (\renderSpriteB -> checkSprite renderSpriteA renderSpriteB) renderSpritesB) renderSpritesA

        render :: a -> Input -> Canvas.Picture ()
        render = Collidable.defaultRender

        defaultRender :: a -> Input -> Canvas.Picture ()
        defaultRender a input@Input{resources, isDebugEnabled} = do
            Sprite.render a input
            when isDebugEnabled $ do
                let renderSprites = Sprite.getRenderSprites a resources
                mapM_ (\renderSprite -> Collidable.renderAtPosition renderSprite resources) renderSprites

        renderAtPosition :: a -> Resources -> Canvas.Picture ()
        renderAtPosition a resources = if Sprite.isEnabled a
            then
                do
                    renderCollisionCrosshair a
                    renderCollisionCircle a resources
                    renderCollisionPolygon a resources
            else
                Renderer.doNothing

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