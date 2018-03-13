{-# LANGUAGE RecordWildCards #-}

module Modes.GameMode.Entities.SpinningRectangles where

    import Point
    import Entity
    import Modes.GameMode.Entities.SpinningRectangle as SpinningRectangle
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
            SpinningRectangles { position = Point { x = 0, y = 0 }
                               , rotation = 0
                               , speed = 0
                               , children = [spinningRectangle1, spinningRectangle2, spinningRectangle3] }

    getValue :: (Input -> Bool) -> Input -> Double
    getValue keyGetter input@Input{..} = if keyGetter input then 0.5 * deltaTime else 0

    instance EntityClass SpinningRectangles where

        update spinningRectangles@SpinningRectangles{..} input@Input{..} = 
            let
                updatedRotation = rotation + (speed * deltaTime)

                leftValue = getValue Input.left input
                rightValue = getValue Input.right input
                upValue = getValue Input.up input
                downValue = getValue Input.down input
                horizontalDelta = rightValue - leftValue
                verticalDelta = downValue - upValue
                updatedPosition = Point.Point { x = (Point.x position) + horizontalDelta, y = (Point.y position) + verticalDelta }

                updatedChildren = Entity.updateAll children input
            in
                Entity $ spinningRectangles { Modes.GameMode.Entities.SpinningRectangles.rotation = updatedRotation
                                            , Modes.GameMode.Entities.SpinningRectangles.children = updatedChildren
                                            , Modes.GameMode.Entities.SpinningRectangles.position = updatedPosition }

        render spinningRectangles@SpinningRectangles{..} = 
            let
                renderedChildren = Entity.renderAll children
                rotatedChildren = Canvas.rotate rotation renderedChildren
                translatedChildren = Canvas.translate (Point.x position, Point.y position) rotatedChildren
            in
                translatedChildren