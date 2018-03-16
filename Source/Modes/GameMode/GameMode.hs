{-# LANGUAGE RecordWildCards #-}

module Modes.GameMode.GameMode(
    imageDefs, 
    GameMode(GameMode),
    Modes.GameMode.GameMode.new) where

    import Entity
    import CommonEntities.Background as Background
    import CommonEntities.Fps as Fps
    import Modes.GameMode.Entities.SpinningRectangles as SpinningRectangles
    import Point
    import Input
    import Resources

    imageDefs :: [Resources.ResourceDef]
    imageDefs = [ (ResourceKey "Spaceship", "Resources/Spaceship.png")
                , (ResourceKey "Bullet", "Resources/Bullet.png")
                , (ResourceKey "Big", "Resources/Big.bmp")
                ]

    data GameMode = GameMode { children :: [Entity] }

    new :: GameMode
    new = GameMode { children = [ Entity $ Background.new
                                , Entity $ SpinningRectangles.new { SpinningRectangles.position = Point.Point { Point.x = 400, Point.y = 300 }, SpinningRectangles.speed = 0.0005 }
                                , Entity $ Fps.new
                                ] }

    instance EntityClass GameMode where

        update gameMode@GameMode{..} input@Input{..} = Entity $ gameMode { Modes.GameMode.GameMode.children = Entity.updateAll children input }

        render gameMode@GameMode{..} resources = Entity.renderAll children resources