{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Gun(
    Gun(),
    Modes.GameMode.Entities.Gun.new,
    Modes.GameMode.Entities.Gun.update'
    ) where

    import Modes.GameMode.Entities.Bullet as Bullet
    import Input
    import Entity
    import Point
    import Resources
    import Constants
    import Keyboard
    import Data.Map

    data Gun = Gun { bullets :: [Bullet], timeCount :: Double, lastFiredTime :: Double }

    maxBullets :: Int
    maxBullets = 50

    new :: Gun
    new = Gun { bullets = [], timeCount = 0, lastFiredTime = 0 }

    intervalBetweenFiring :: Double
    intervalBetweenFiring = 300.0

    isBulletOutOfBounds :: Bullet -> Double -> Double -> Bool
    isBulletOutOfBounds Bullet{position} bulletWidth bulletHeight = 
        let
            bulletX = Point.x position
            bulletY = Point.y position
        in
            bulletX < -bulletWidth || bulletX > Constants.nativeWidth + bulletWidth || bulletY < -bulletHeight || bulletY > Constants.nativeHeight + bulletHeight

    update' :: Gun -> Input -> Gun
    update' gun@Gun{bullets, timeCount, lastFiredTime} input@Input{keyboard, deltaTime, resources} =
        let
            updatedTimeCount = timeCount + deltaTime
            updatedBullets = Prelude.map (\bullet -> Bullet.update' bullet input) bullets

            isFiring = Keyboard.action keyboard && updatedTimeCount - lastFiredTime > intervalBetweenFiring
            updatedLastFiredTime = if isFiring then updatedTimeCount else lastFiredTime

            withAddedBullets = if isFiring
                then 
                    Bullet.new { Bullet.position = Point { x = 400, y = 400 }, Bullet.velocity = Point { x = 0.1, y = 0.2 } } : updatedBullets
                else
                    updatedBullets

            images = Resources.images resources
            (_, (bulletWidth, bulletHeight)) = images ! (fst Bullet.imageDef)

            withoutRemovedBullets = Prelude.filter (\bullet -> not $ isBulletOutOfBounds bullet bulletWidth bulletHeight) withAddedBullets
        in
            gun { bullets = withoutRemovedBullets, timeCount = updatedTimeCount, lastFiredTime = updatedLastFiredTime }

    instance EntityClass Gun where

        load _ = [Bullet.imageDef]

        update gun input = Entity $ Modes.GameMode.Entities.Gun.update' gun input

        render Gun{bullets} resources = Entity.renderAll (Prelude.map Entity bullets) resources

