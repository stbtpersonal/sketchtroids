module Entities.SpinningRectangles where

    import Point
    import Entity
    import Entities.SpinningRectangle as SpinningRectangle
    import Haste.Graphics.Canvas as Canvas
    import Input

    data SpinningRectangles = SpinningRectangles { position :: Point.Point, rotation :: Double, speed :: Double, children :: [Entity] }

    new :: SpinningRectangles
    new =
        let
            spinningRectangle1 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 50, Point.y = 50 }, SpinningRectangle.speed = 0.001 }
            spinningRectangle2 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 150, Point.y = 50 }, SpinningRectangle.speed = 0.002 }
            spinningRectangle3 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 50, Point.y = 150 }, SpinningRectangle.speed = 0.003 }
        in
            SpinningRectangles { Entities.SpinningRectangles.position = Point { x = 0, y = 0 }
                               , Entities.SpinningRectangles.rotation = 0
                               , Entities.SpinningRectangles.speed = 0
                               , Entities.SpinningRectangles.children = [spinningRectangle1, spinningRectangle2, spinningRectangle3] }

    getValue :: (Input -> Bool) -> Input -> Double
    getValue keyGetter input = if keyGetter input then 0.5 * (Input.deltaTime input) else 0

    instance EntityClass SpinningRectangles where

        update spinningRectangles input = 
            let
                deltaTime = Input.deltaTime input
                position = Entities.SpinningRectangles.position spinningRectangles
                rotation = Entities.SpinningRectangles.rotation spinningRectangles
                speed = Entities.SpinningRectangles.speed spinningRectangles
                updatedRotation = rotation + (speed * deltaTime)

                leftValue = getValue Input.left input
                rightValue = getValue Input.right input
                upValue = getValue Input.up input
                downValue = getValue Input.down input
                horizontalDelta = rightValue - leftValue
                verticalDelta = downValue - upValue
                updatedPosition = Point.Point { x = (Point.x position) + horizontalDelta, y = (Point.y position) + verticalDelta }

                updatedChildren = Entity.updateAll (Entities.SpinningRectangles.children spinningRectangles) input
            in
                Entity $ spinningRectangles { Entities.SpinningRectangles.rotation = updatedRotation, Entities.SpinningRectangles.children = updatedChildren, Entities.SpinningRectangles.position = updatedPosition }

        render spinningRectangles = 
            let
                rotation = Entities.SpinningRectangles.rotation spinningRectangles
                position = Entities.SpinningRectangles.position spinningRectangles

                renderedChildren = Entity.renderAll (Entities.SpinningRectangles.children spinningRectangles)
                rotatedChildren = Canvas.rotate rotation renderedChildren
                translatedChildren = Canvas.translate (Point.x position, Point.y position) rotatedChildren
            in
                translatedChildren