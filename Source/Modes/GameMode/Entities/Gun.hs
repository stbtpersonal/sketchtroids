{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Gun(
    Gun(),
    Modes.GameMode.Entities.Gun.new,
    Modes.GameMode.Entities.Gun.update',
    setCoordinates
    ) where

    import Modes.GameMode.Entities.Bullet as Bullet
    import Input
    import Entity
    import Point
    import Resources
    import Constants
    import Keyboard
    import Data.Map

    data Gun = Gun { bullets :: [Bullet], timeCount :: Double, lastFiredTime :: Double, position :: Point.Point, rotation :: Double }

    maxBullets :: Int
    maxBullets = 50

    new :: Gun
    new = Gun { bullets = []
              , timeCount = 0
              , lastFiredTime = 0
              , Modes.GameMode.Entities.Gun.position = Point { x = 800, y = 800 }
              , Modes.GameMode.Entities.Gun.rotation = 0
              }

    intervalBetweenFiring :: Double
    intervalBetweenFiring = 300.0

    setCoordinates :: Gun -> Point -> Double -> Gun
    setCoordinates gun position rotation = gun { Modes.GameMode.Entities.Gun.position = position, rotation = rotation }

    isBulletOutOfBounds :: Bullet -> Double -> Double -> Bool
    isBulletOutOfBounds Bullet{Bullet.position} bulletWidth bulletHeight = 
        let
            bulletX = Point.x position
            bulletY = Point.y position
        in
            bulletX < -bulletWidth || bulletX > Constants.nativeWidth + bulletWidth || bulletY < -bulletHeight || bulletY > Constants.nativeHeight + bulletHeight

    update' :: Gun -> Input -> Gun
    update' gun@Gun{bullets, timeCount, lastFiredTime, Modes.GameMode.Entities.Gun.position, rotation} input@Input{keyboard, deltaTime, resources} =
        let
            updatedTimeCount = timeCount + deltaTime
            updatedBullets = Prelude.map (\bullet -> Bullet.update' bullet input) bullets

            isFiring = Keyboard.action keyboard && updatedTimeCount - lastFiredTime > intervalBetweenFiring
            updatedLastFiredTime = if isFiring then updatedTimeCount else lastFiredTime

            withAddedBullets = if isFiring
                then Bullet.new { Bullet.position = position, Bullet.velocity = Point.fromAngle rotation } : updatedBullets
                else updatedBullets

            images = Resources.images resources
            (_, (bulletWidth, bulletHeight)) = images ! (fst Bullet.imageDef)

            withoutRemovedBullets = Prelude.filter (\bullet -> not $ isBulletOutOfBounds bullet bulletWidth bulletHeight) withAddedBullets
        in
            gun { bullets = withoutRemovedBullets, timeCount = updatedTimeCount, lastFiredTime = updatedLastFiredTime }

    instance EntityClass Gun where

        load _ = [Bullet.imageDef]

        update gun input = Entity $ Modes.GameMode.Entities.Gun.update' gun input

        render Gun{bullets} resources = Entity.renderAll (Prelude.map Entity bullets) resources

