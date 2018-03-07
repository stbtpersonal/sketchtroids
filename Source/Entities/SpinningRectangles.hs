module Entities.SpinningRectangles where

    import Point
    import Entity
    import Entities.SpinningRectangle as SpinningRectangle
    import Haste.Graphics.Canvas as Canvas

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

    instance EntityClass SpinningRectangles where

        update spinningRectangles deltaTime = 
            let
                rotation = Entities.SpinningRectangles.rotation spinningRectangles
                speed = Entities.SpinningRectangles.speed spinningRectangles
                updatedRotation = rotation + (speed * deltaTime)

                updatedChildren = Entity.updateAll (Entities.SpinningRectangles.children spinningRectangles) deltaTime
            in
                Entity $ spinningRectangles { Entities.SpinningRectangles.rotation = updatedRotation, Entities.SpinningRectangles.children = updatedChildren }

        render spinningRectangles = 
            let
                rotation = Entities.SpinningRectangles.rotation spinningRectangles
                position = Entities.SpinningRectangles.position spinningRectangles

                renderedChildren = Entity.renderAll (Entities.SpinningRectangles.children spinningRectangles)
                rotatedChildren = Canvas.rotate rotation renderedChildren
                translatedChildren = Canvas.translate (Point.x position, Point.y position) rotatedChildren
            in
                translatedChildren