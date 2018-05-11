{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Ship
    ( Ship(Ship)
    , Modes.GameMode.Entities.Ship.new
    , imageDef
    , Modes.GameMode.Entities.Ship.update'
    ) where

    import Point
    import Constants
    import Entity
    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas
    import Keyboard
    import Utils
    import Control.Monad
    import Modes.GameMode.Entities.Gun as Gun
    import Sprite
    import Collidable

    data Ship = Ship
        { position :: Point.Point
        , velocity :: Point.Point
        , rotation :: Double
        , rotationVelocity :: Double
        , gun :: Gun 
        }

    new :: Ship
    new = Ship
        { position = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        , velocity = Point { x = 0, y = 0 }
        , rotation = 0
        , rotationVelocity = 0
        , gun = Gun.new
        }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Ship", "Resources/Ship.png")

    rotationAcceleration :: Double
    rotationAcceleration = 0.00002

    rotationDecelerationLerp :: Double
    rotationDecelerationLerp = 0.01

    maxRotationVelocity :: Double
    maxRotationVelocity = 0.01

    accelerationForward :: Double
    accelerationForward = 0.0005

    accelerationBackward :: Double
    accelerationBackward = 0.0002

    maxVelocityForward :: Double
    maxVelocityForward = 0.5

    maxVelocityBackward :: Double
    maxVelocityBackward = -0.2

    getValue :: (Keyboard -> Bool) -> Keyboard -> Double -> Double -> Double
    getValue keyGetter keyboard deltaTime multiplier = if keyGetter keyboard then multiplier * deltaTime else 0

    update' :: Ship -> Input -> Ship
    update' ship@Ship{position, velocity, rotation, rotationVelocity, gun} input@Input{deltaTime, keyboard, resources} =
        let
            leftValue = getValue Keyboard.left keyboard deltaTime rotationAcceleration
            rightValue = getValue Keyboard.right keyboard deltaTime rotationAcceleration
            upValue = getValue Keyboard.up keyboard deltaTime accelerationForward
            downValue = getValue Keyboard.down keyboard deltaTime accelerationBackward
            rotationValueDelta = rightValue - leftValue
            positionValueDelta = downValue - upValue

            rotation' = rotation + (rotationVelocity * deltaTime)
            rotationVelocity' = Utils.clamp (-maxRotationVelocity) maxRotationVelocity (Utils.lerp (rotationVelocity + rotationValueDelta) 0 rotationDecelerationLerp)

            nextX = (Point.x position) + (Point.x velocity * deltaTime)
            nextY = (Point.y position) + (Point.y velocity * deltaTime)
            position' = Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }
            fowardAngle = rotation' + (pi / 2)
            accelerationX = positionValueDelta * cos fowardAngle
            accelerationY = positionValueDelta * sin fowardAngle
            nextVelocity = Point { x = (Point.x velocity) + accelerationX, y = (Point.y velocity) + accelerationY }
            velocity' = Point.clamp maxVelocityBackward maxVelocityForward nextVelocity

            gunAngle = rotation' - (pi / 2)
            images = Resources.images resources
            (_, (_, height)) = images ! (fst imageDef)
            gunUnitVector = Point.fromAngle gunAngle
            gunVector = Point { x = (Point.x gunUnitVector) * (height / 2), y = (Point.y gunUnitVector) * (height / 2) }
            gunPosition = Point { x = (Point.x position') + (Point.x gunVector), y = (Point.y position') + (Point.y gunVector) }
            wrappedGunPosition = Point { x = Utils.wrap 0 Constants.nativeWidth (Point.x gunPosition), y = Utils.wrap 0 Constants.nativeHeight (Point.y gunPosition) }
            coordinatesSetGun = Gun.setCoordinates gun wrappedGunPosition gunAngle
            gun' = Gun.update' coordinatesSetGun input
        in
            ship
                { rotation = rotation'
                , rotationVelocity = rotationVelocity'
                , position = position'
                , velocity = velocity'
                , gun = gun'
                }

    instance EntityClass Ship where

        load Ship{gun} = Entity.load gun ++ [imageDef]

        update ship input = Entity $ Modes.GameMode.Entities.Ship.update' ship input

        render ship@Ship{position, gun} resources@Resources{images} = do
            Sprite.drawAtPosition ship resources position

            let (_, (width, height)) = images ! (fst imageDef)
            let x = Point.x position
            let y = Point.y position
            when (x < width / 2) (Sprite.drawAtPosition ship resources Point { x = Constants.nativeWidth + x, y = y })
            when (x > Constants.nativeWidth - (width / 2)) (Sprite.drawAtPosition ship resources Point { x = x - Constants.nativeWidth, y = y })
            when (y < height / 2) (Sprite.drawAtPosition ship resources Point { x = x, y = Constants.nativeHeight + y })
            when (y > Constants.nativeHeight - (height / 2)) (Sprite.drawAtPosition ship resources Point { x = x, y = y - Constants.nativeHeight })

            Entity.render gun resources

    instance Sprite Ship where
        imageDef' _ = imageDef
        position' Ship{position} = position
        rotation' Ship{rotation} = rotation

    instance Collidable Ship