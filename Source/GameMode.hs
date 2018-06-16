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
    import Asteroids
    import Collidable
    import PressToStartText (PressToStartText, new)
    import Sprite (update, isEnabled, setEnabled)
    import Gun (Gun, new, getCollisions, removeBullets)
    import Utils

    data GameMode = GameMode
        { background :: Background
        , ship :: Ship
        , gun :: Gun
        , asteroids :: Asteroids
        , fps :: Fps
        , pressToStartText :: PressToStartText
        , asteroidAmount :: Int
        }

    new :: GameMode
    new = GameMode
        { background = Background.new
        , ship = Ship.new
        , gun = Gun.new
        , asteroids = Asteroids.new 0
        , fps = Fps.new
        , pressToStartText = PressToStartText.new
        , asteroidAmount = initialAsteroidAmount
        }

    initialAsteroidAmount :: Int
    initialAsteroidAmount = 3

    children :: GameMode -> [Entity]
    children GameMode{background, ship, gun, asteroids, fps, pressToStartText} = [Entity background, Entity ship, Entity gun, Entity asteroids, Entity fps, Entity pressToStartText]

    imageDefs :: [Resources.ResourceDef]
    imageDefs = Entity.loadAll $ children GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, gun, asteroids, fps, pressToStartText, asteroidAmount} input@Input{resources} = 
            let
                fps' = Fps.update' fps input

                pressToStartText' = if (not $ Sprite.isEnabled pressToStartText) && (Ship.hadExploded ship)
                    then PressToStartText.new
                    else pressToStartText

                pressToStartText'' = Sprite.update pressToStartText' input
                shouldShipSpawn = ((not $ Sprite.isEnabled ship) || (Ship.hadExploded ship)) && (not $ Sprite.isEnabled pressToStartText'')
                ship' = if shouldShipSpawn then Sprite.setEnabled Ship.new True else ship
                gun' = if shouldShipSpawn then Sprite.setEnabled Gun.new True else gun
                asteroidAmount' = if shouldShipSpawn then initialAsteroidAmount else asteroidAmount

                shouldAsteroidsSpawn = shouldShipSpawn || ((Sprite.isEnabled ship) && (not $ Asteroids.areAnyEnabled asteroids))
                asteroids' = if shouldAsteroidsSpawn then Asteroids.new asteroidAmount' else asteroids

                ship'' = Ship.update' ship' input
                gun'' = Ship.updateGun gun' ship'' input
                asteroids'' = Asteroids.update' asteroids' input

                hasShipCollided = not $ null $ Asteroids.getCollisions asteroids'' ship'' resources
                ship''' = if not hasShipCollided then ship'' else Ship.explode ship''

                collidedPairs = Asteroids.getGunCollisions asteroids'' gun'' resources
                gun''' = Gun.removeBullets gun'' $ map snd collidedPairs
                asteroids''' = Asteroids.receiveHits asteroids'' $ map fst collidedPairs

                !asteroidAmount'' = if (Sprite.isEnabled ship) && (not $ Asteroids.areAnyEnabled asteroids''') then asteroidAmount' + 1 else asteroidAmount'
            in
                Entity $ gameMode
                    { ship = ship'''
                    , gun = gun'''
                    , asteroids = asteroids'''
                    , fps = fps'
                    , pressToStartText = pressToStartText''
                    , asteroidAmount = asteroidAmount''
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources