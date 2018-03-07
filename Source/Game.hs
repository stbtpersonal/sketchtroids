module Game where

    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Entities.Background as Background
    import Entities.Fps as Fps
    import Entities.SpinningRectangles as SpinningRectangles
    import Point

    data Game = Game { canvas :: Canvas, timestamp :: Double, entities :: [Entity] }

    new :: Canvas -> Double -> Double -> Game
    new canvas width height =
        let
            background = Entity $ Background.new width height
            spinningRectangles = Entity $ SpinningRectangles.new { SpinningRectangles.position = Point.Point { Point.x = 400, Point.y = 300 }, SpinningRectangles.speed = 0.0005 }
            fps = Entity $ Fps.new
        in
            Game { canvas = canvas, timestamp = 0, entities = [background, spinningRectangles, fps] }