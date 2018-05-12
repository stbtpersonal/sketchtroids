{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module GameMode
    ( imageDefs
    , GameMode(GameMode)
    , GameMode.new
    ) where

    import Entity
    import Background
    import Fps
    import Point
    import Input
    import Resources
    import Ship
    import Asteroid
    import Collidable
    import Utils

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
    imageDefs = Entity.loadAll $ children GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, asteroid, fps} input@Input{resources} = 
            let
                ship' = Ship.update' ship input
                asteroid' = Asteroid.update' asteroid input
                fps' = Fps.update' fps input

                haveCollided = Collidable.haveCollided ship' asteroid' resources
                !kaka = Utils.unsafeWriteLog $ "AAA " ++ (show haveCollided)
            in
                Entity $ gameMode
                    { ship = ship'
                    , asteroid = asteroid'
                    , fps = fps'
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources