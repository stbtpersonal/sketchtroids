{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.GameMode(
    imageDefs, 
    GameMode(GameMode),
    Modes.GameMode.GameMode.new) where

    import Entity
    import CommonEntities.Background as Background
    import CommonEntities.Fps as Fps
    import Point
    import Input
    import Resources
    import Modes.GameMode.Entities.Ship as Ship
    import Modes.GameMode.Entities.Bullet as Bullet

    data GameMode = GameMode { children :: [Entity] }

    new :: GameMode
    new = GameMode { Modes.GameMode.GameMode.children = [ Entity $ Background.new
                                                        , Entity $ Ship.new
                                                        , Entity $ Fps.new
                                                        , Entity $ Bullet.new { Bullet.position = Point { x = 50, y = 50 }, Bullet.velocity = Point { x = 0.02, y = 0.01 } }
                                                        ] }

    imageDefs :: [Resources.ResourceDef]
    imageDefs =
        let
            GameMode{Modes.GameMode.GameMode.children} = Modes.GameMode.GameMode.new
        in
            Entity.loadAll children

    instance EntityClass GameMode where

        update gameMode@GameMode{Modes.GameMode.GameMode.children} input = Entity $ gameMode { Modes.GameMode.GameMode.children = Entity.updateAll children input }

        render GameMode{Modes.GameMode.GameMode.children} resources = Entity.renderAll children resources