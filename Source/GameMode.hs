{-# LANGUAGE RecordWildCards #-}

module GameMode where

    import Entity
    import Entities.Background as Background
    import Entities.Fps as Fps
    import Entities.SpinningRectangles as SpinningRectangles
    import Point
    import Input

    data GameMode = GameMode { children :: [Entity] }

    new :: GameMode
    new = GameMode { GameMode.children = [ Entity $ Background.new
                                         , Entity $ SpinningRectangles.new { SpinningRectangles.position = Point.Point { Point.x = 400, Point.y = 300 }, SpinningRectangles.speed = 0.0005 }
                                         , Entity $ Fps.new
                                         ] }

    instance EntityClass GameMode where

        update gameMode@GameMode{..} input@Input{..} = Entity $ gameMode { GameMode.children = Entity.updateAll children input }

        render gameMode@GameMode{..} = Entity.renderAll children