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
    import Asteroids
    import Collidable
    import PressToStartText (PressToStartText, new)
    import Sprite (update, isEnabled, setEnabled)
    import Gun (Gun, new, getCollisions, removeBullets)
    import Utils
    import Explosion
    import Score

    data GameMode = GameMode
        { background :: Background
        , ship :: Ship
        , gun :: Gun
        , asteroids :: Asteroids
        , fps :: Fps
        , pressToStartText :: PressToStartText
        , asteroidAmount :: Int
        , explosions :: [Explosion]
        , score :: Score
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
        , explosions = []
        , score = Sprite.setEnabled Score.new False
        }

    initialAsteroidAmount :: Int
    initialAsteroidAmount = 3

    children :: GameMode -> [Entity]
    children GameMode{background, score, ship, gun, asteroids, fps, pressToStartText, explosions} =
        let
            discreteEntities = [Entity background, Entity score, Entity ship, Entity gun, Entity asteroids, Entity fps, Entity pressToStartText]
            explosionEntities = map Entity explosions
        in
            discreteEntities ++ explosionEntities

    imageDefs :: [Resources.ResourceDef]
    imageDefs = Entity.loadAll $ children GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, gun, asteroids, fps, pressToStartText, asteroidAmount, explosions, score} input@Input{resources} = 
            let
                fps' = Fps.update' fps input

                pressToStartText' = if (not $ Sprite.isEnabled pressToStartText) && (not $ Sprite.isEnabled ship)
                    then PressToStartText.new
                    else pressToStartText

                pressToStartText'' = Sprite.update pressToStartText' input
                shouldShipSpawn = (not $ Sprite.isEnabled ship) && (not $ Sprite.isEnabled pressToStartText'')
                ship' = if shouldShipSpawn then Sprite.setEnabled Ship.new True else ship
                gun' = if shouldShipSpawn then Sprite.setEnabled Gun.new True else gun
                asteroidAmount' = if shouldShipSpawn then initialAsteroidAmount else asteroidAmount
                score' = if shouldShipSpawn then Score.new else score

                shouldAsteroidsSpawn = shouldShipSpawn || ((Sprite.isEnabled ship) && (not $ Asteroids.areAnyEnabled asteroids))
                asteroids' = if shouldAsteroidsSpawn then Asteroids.new asteroidAmount' else asteroids

                ship'' = Ship.update' ship' input
                gun'' = Ship.updateGun gun' ship'' input
                asteroids'' = Asteroids.update' asteroids' input

                hasShipCollided = not $ null $ Asteroids.getCollisions asteroids'' ship'' resources
                ship''' = if not hasShipCollided then ship'' else Sprite.setEnabled ship'' False
                explosions' = explosions ++ if hasShipCollided then [Ship.explode ship'''] else []

                collidedPairs = Asteroids.getGunCollisions asteroids'' gun'' resources
                gun''' = Gun.removeBullets gun'' $ map snd collidedPairs
                (asteroids''', explodedAsteroids) = Asteroids.receiveHits asteroids'' (map fst collidedPairs) input
                explosions'' = explosions' ++ map Asteroid.explode explodedAsteroids
                scoreToAdd = foldl (\accumulator explodedAsteroid -> accumulator + Asteroid.getScore explodedAsteroid) 0 explodedAsteroids
                score'' = Score.addScore score' scoreToAdd

                !asteroidAmount'' = if (Sprite.isEnabled ship) && (not $ Asteroids.areAnyEnabled asteroids''') then asteroidAmount' + 1 else asteroidAmount'

                explosions''' = map (\explosion -> Sprite.update explosion input) explosions''
                explosions'''' = filter (not . Explosion.isDone) explosions'''
            in
                Entity $ gameMode
                    { ship = ship'''
                    , gun = gun'''
                    , asteroids = asteroids'''
                    , fps = fps'
                    , pressToStartText = pressToStartText''
                    , asteroidAmount = asteroidAmount''
                    , explosions = explosions''''
                    , score = score''
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources