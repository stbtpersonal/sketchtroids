{-# LANGUAGE NamedFieldPuns #-}

module Asteroids where

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

    data Asteroids = Asteroids
        { _asteroids :: [Asteroid]
        ,  _isEnabled :: Bool
        }

    new :: Int -> Asteroids
    new amount = Asteroids 
        { _asteroids = take amount $ repeat Asteroid.new
        , _isEnabled = False
        }

    getCollisions :: Collidable a => Asteroids -> a -> Resources -> [Asteroid]
    getCollisions asteroids@Asteroids{_asteroids} collidable resources = filter (\asteroid -> Collidable.haveCollided asteroid collidable resources) _asteroids

    getGunCollisions :: Asteroids -> Gun -> Resources -> [(Asteroid, Bullet)]
    getGunCollisions asteroids@Asteroids{_asteroids} gun resources = 
        let
            asteroidsAndBulletLists = map (\asteroid -> (asteroid, Gun.getCollisions gun asteroid resources)) _asteroids
        in
            concat $ map (\(asteroid, bullets) -> map (\bullet -> (asteroid, bullet)) bullets) asteroidsAndBulletLists

    receiveHits :: Asteroids -> [Asteroid] -> Asteroids
    receiveHits asteroids@Asteroids{_asteroids} toReceiveHit =
        let
            asteroids' = _asteroids \\ toReceiveHit
            hitAsteroids = map Asteroid.receiveHit toReceiveHit
            asteroids'' = asteroids' ++ hitAsteroids
        in
            asteroids{_asteroids = asteroids''}

    update' :: Asteroids -> Input -> Asteroids
    update' asteroids@Asteroids{_asteroids, _isEnabled} input = if _isEnabled
        then
            let
                asteroids' = map (\asteroid -> Asteroid.update' asteroid input) _asteroids
            in
                asteroids {_asteroids = asteroids'}
        else
            asteroids

    instance EntityClass Asteroids where
        load asteroids = Sprite.imageDefs asteroids
        update asteroids input = Entity $ Asteroids.update' asteroids input
        render Asteroids{_asteroids} resources = Entity.renderAll (map Entity _asteroids) resources

    instance Sprite Asteroids where
        imageDef _ = Asteroid.imageDef'
        isEnabled Asteroids{_isEnabled} = _isEnabled
        setEnabled asteroids enabled = asteroids{_isEnabled = enabled}