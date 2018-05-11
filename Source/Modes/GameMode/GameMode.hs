{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.GameMode
    ( imageDefs
    , GameMode(GameMode)
    , Modes.GameMode.GameMode.new
    ) where

    import Entity
    import CommonEntities.Background as Background
    import CommonEntities.Fps as Fps
    import Point
    import Input
    import Resources
    import Modes.GameMode.Entities.Ship as Ship
    import Modes.GameMode.Entities.Asteroid as Asteroid

    data GameMode = GameMode
        { background :: Background
        , ship :: Ship
        , asteroid :: Asteroid
        , fps :: Fps
        }

    new :: GameMode
    new = GameMode
        { background = Background.new
        , ship = Ship.new
        , asteroid = Asteroid.new
        , fps = Fps.new
        }

    children :: GameMode -> [Entity]
    children GameMode{background, ship, asteroid, fps} = [Entity background, Entity ship, Entity asteroid, Entity fps]

    imageDefs :: [Resources.ResourceDef]
    imageDefs = Entity.loadAll $ children Modes.GameMode.GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, asteroid, fps} input = 
            let
                ship' = Ship.update' ship input
                asteroid' = Asteroid.update' asteroid input
                fps' = Fps.update' fps input
            in
                Entity $ gameMode
                    { ship = ship'
                    , asteroid = asteroid'
                    , fps = fps'
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources