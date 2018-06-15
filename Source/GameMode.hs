{-# LANGUAGE NamedFieldPuns #-}

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
    import PressToStartText (PressToStartText, new)
    import Sprite (update, isEnabled, setEnabled)
    import Gun (Gun, new, setEnabled)

    data GameMode = GameMode
        { background :: Background
        , ship :: Ship
        , gun :: Gun
        , asteroid :: Asteroid
        , fps :: Fps
        , pressToStartText :: PressToStartText
        }

    new :: GameMode
    new = GameMode
        { background = Background.new
        , ship = Ship.new
        , gun = Gun.new
        , asteroid = Asteroid.new
        , fps = Fps.new
        , pressToStartText = PressToStartText.new
        }

    children :: GameMode -> [Entity]
    children GameMode{background, ship, gun, asteroid, fps, pressToStartText} = [Entity background, Entity ship, Entity gun, Entity asteroid, Entity fps, Entity pressToStartText]

    imageDefs :: [Resources.ResourceDef]
    imageDefs = Entity.loadAll $ children GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, gun, asteroid, fps, pressToStartText} input@Input{resources} = 
            let
                fps' = Fps.update' fps input

                pressToStartText' = if (not $ Sprite.isEnabled pressToStartText) && (Ship.hadExploded ship)
                    then PressToStartText.new
                    else pressToStartText

                pressToStartText'' = Sprite.update pressToStartText' input
                shouldSpawn = ((not $ Sprite.isEnabled ship) || (Ship.hadExploded ship)) && (not $ Sprite.isEnabled pressToStartText'')
                ship' = if shouldSpawn then Sprite.setEnabled Ship.new True else ship
                gun' = if shouldSpawn then Gun.setEnabled Gun.new True else gun
                asteroid' = if shouldSpawn then Sprite.setEnabled Asteroid.new True else asteroid

                ship'' = Ship.update' ship' input
                gun'' = Ship.updateGun gun' ship'' input
                asteroid'' = Asteroid.update' asteroid' input

                haveCollided = Collidable.haveCollided ship'' asteroid'' resources
                ship''' = if not haveCollided then ship'' else Ship.explode ship''
            in
                Entity $ gameMode
                    { ship = ship'''
                    , gun = gun''
                    , asteroid = asteroid''
                    , fps = fps'
                    , pressToStartText = pressToStartText''
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources