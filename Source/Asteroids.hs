{-# LANGUAGE NamedFieldPuns #-}

module Asteroids
    ( Asteroids()
    , Asteroids.new
    , Asteroids.areAnyEnabled
    , Asteroids.getCollisions
    , Asteroids.getGunCollisions
    , Asteroids.receiveHits
    , Asteroids.update'
    ) where

    import Asteroid
    import Entity
    import Resources
    import Input
    import Sprite
    import Collidable
    import Gun
    import Bullet
    import Data.List
    import Utils
    import Explosion

    data Asteroids = Asteroids
        { _asteroids :: [Asteroid]
        }

    new :: Int -> Asteroids
    new amount = Asteroids 
        { _asteroids = take amount $ repeat Asteroid.new
        }

    areAnyEnabled :: Asteroids -> Bool
    areAnyEnabled Asteroids{_asteroids} = (not $ null $ _asteroids) && (any Sprite.isEnabled _asteroids)

    getCollisions :: Collidable a => Asteroids -> a -> Resources -> [Asteroid]
    getCollisions asteroids@Asteroids{_asteroids} collidable resources = filter (\asteroid -> Collidable.haveCollided asteroid collidable resources) _asteroids

    getGunCollisions :: Asteroids -> Gun -> Resources -> [(Asteroid, Bullet)]
    getGunCollisions asteroids@Asteroids{_asteroids} gun resources = 
        let
            asteroidsAndBulletLists = map (\asteroid -> (asteroid, Gun.getCollisions gun asteroid resources)) _asteroids
        in
            concat $ map (\(asteroid, bullets) -> map (\bullet -> (asteroid, bullet)) bullets) asteroidsAndBulletLists

    receiveHits :: Asteroids -> [Asteroid] -> Input -> (Asteroids, [Asteroid])
    receiveHits asteroids@Asteroids{_asteroids} toReceiveHit input =
        let
            asteroids' = map (\asteroid -> if asteroid `elem` toReceiveHit then Asteroid.receiveHit asteroid else asteroid) _asteroids

            disabledAsteroids = filter (not . Sprite.isEnabled) asteroids'
            fragments = concat $ map (\asteroid -> Asteroid.break asteroid input) disabledAsteroids
            asteroids'' = (asteroids' \\ disabledAsteroids) ++ fragments
        in
            ( asteroids{_asteroids = asteroids''}
            , disabledAsteroids
            )

    update' :: Asteroids -> Input -> Asteroids
    update' asteroids@Asteroids{_asteroids} input =
        let
            randomizedInputs = Input.randomize input
            asteroids' = zipWith (\asteroid randomizedInput -> Asteroid.update' asteroid randomizedInput) _asteroids randomizedInputs
        in
            asteroids {_asteroids = asteroids'}

    instance EntityClass Asteroids where
        load asteroids = Asteroid.imageDefs' ++ Asteroid.collisionDefs'
        update asteroids input = Entity $ Asteroids.update' asteroids input
        render Asteroids{_asteroids} input = Entity.renderAll (map Entity _asteroids) input