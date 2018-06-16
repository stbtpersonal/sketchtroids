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
    import Asteroids
    import Collidable
    import PressToStartText (PressToStartText, new)
    import Sprite (update, isEnabled, setEnabled)
    import Gun (Gun, new, getCollisions, removeBullets)

    data GameMode = GameMode
        { background :: Background
        , ship :: Ship
        , gun :: Gun
        , asteroids :: Asteroids
        , fps :: Fps
        , pressToStartText :: PressToStartText
        }

    new :: GameMode
    new = GameMode
        { background = Background.new
        , ship = Ship.new
        , gun = Gun.new
        , asteroids = Asteroids.new 5
        , fps = Fps.new
        , pressToStartText = PressToStartText.new
        }

    children :: GameMode -> [Entity]
    children GameMode{background, ship, gun, asteroids, fps, pressToStartText} = [Entity background, Entity ship, Entity gun, Entity asteroids, Entity fps, Entity pressToStartText]

    imageDefs :: [Resources.ResourceDef]
    imageDefs = Entity.loadAll $ children GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, gun, asteroids, fps, pressToStartText} input@Input{resources} = 
            let
                fps' = Fps.update' fps input

                pressToStartText' = if (not $ Sprite.isEnabled pressToStartText) && (Ship.hadExploded ship)
                    then PressToStartText.new
                    else pressToStartText

                pressToStartText'' = Sprite.update pressToStartText' input
                shouldSpawn = ((not $ Sprite.isEnabled ship) || (Ship.hadExploded ship)) && (not $ Sprite.isEnabled pressToStartText'')
                ship' = if shouldSpawn then Sprite.setEnabled Ship.new True else ship
                gun' = if shouldSpawn then Sprite.setEnabled Gun.new True else gun
                asteroids' = if shouldSpawn then Sprite.setEnabled (Asteroids.new 5) True else asteroids

                ship'' = Ship.update' ship' input
                gun'' = Ship.updateGun gun' ship'' input
                asteroids'' = Asteroids.update' asteroids' input

                hasShipCollided = not $ null $ Asteroids.getCollisions asteroids'' ship'' resources
                ship''' = if not hasShipCollided then ship'' else Ship.explode ship''

                collidedPairs = Asteroids.getGunCollisions asteroids'' gun'' resources
                gun''' = Gun.removeBullets gun'' $ map snd collidedPairs
                asteroids''' = Asteroids.receiveHits asteroids'' $ map fst collidedPairs
            in
                Entity $ gameMode
                    { ship = ship'''
                    , gun = gun'''
                    , asteroids = asteroids'''
                    , fps = fps'
                    , pressToStartText = pressToStartText''
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources