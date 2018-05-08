{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Ship(
    Ship(Ship),
    Modes.GameMode.Entities.Ship.new,
    imageDef
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

    data Ship = Ship { position :: Point.Point, velocity :: Point.Point, rotation :: Double, rotationVelocity :: Double, gun :: Gun }

    new :: Ship
    new = Ship { position = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
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

    drawAtPosition :: Ship -> Resources -> Point.Point -> Canvas.Picture ()
    drawAtPosition ship@Ship{rotation} Resources{images} Point{x, y} =
        let
            (bitmap, (width, height)) = images ! (fst imageDef)
            drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
            rotatedSprite = Canvas.rotate rotation drawnSprite
            translatedSprite = Canvas.translate (x, y) rotatedSprite
        in
            translatedSprite

    instance EntityClass Ship where

        load Ship{gun} = Entity.load gun ++ [imageDef]

        update ship@Ship{position, velocity, rotation, rotationVelocity, gun} input@Input{deltaTime, keyboard, resources} =
            let
                leftValue = getValue Keyboard.left keyboard deltaTime rotationAcceleration
                rightValue = getValue Keyboard.right keyboard deltaTime rotationAcceleration
                upValue = getValue Keyboard.up keyboard deltaTime accelerationForward
                downValue = getValue Keyboard.down keyboard deltaTime accelerationBackward
                rotationValueDelta = rightValue - leftValue
                positionValueDelta = downValue - upValue

                updatedRotation = rotation + (rotationVelocity * deltaTime)
                updatedRotationVelocity = Utils.clamp (-maxRotationVelocity) maxRotationVelocity (Utils.lerp (rotationVelocity + rotationValueDelta) 0 rotationDecelerationLerp)

                nextX = (Point.x position) + (Point.x velocity * deltaTime)
                nextY = (Point.y position) + (Point.y velocity * deltaTime)
                updatedPosition = Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }
                fowardAngle = updatedRotation + (pi / 2)
                accelerationX = positionValueDelta * cos fowardAngle
                accelerationY = positionValueDelta * sin fowardAngle
                nextVelocity = Point { x = (Point.x velocity) + accelerationX, y = (Point.y velocity) + accelerationY }
                updatedVelocity = Point.clamp maxVelocityBackward maxVelocityForward nextVelocity

                gunAngle = updatedRotation - (pi / 2)
                images = Resources.images resources
                (_, (_, height)) = images ! (fst imageDef)
                gunUnitVector = Point.fromAngle gunAngle
                gunVector = Point { x = (Point.x gunUnitVector) * (height / 2), y = (Point.y gunUnitVector) * (height / 2) }
                gunPosition = Point { x = (Point.x updatedPosition) + (Point.x gunVector), y = (Point.y updatedPosition) + (Point.y gunVector) }
                coordinatesSetGun = Gun.setCoordinates gun gunPosition gunAngle
                updatedGun = Gun.update' coordinatesSetGun input
            in
                Entity $ ship { rotation = updatedRotation, rotationVelocity = updatedRotationVelocity, position = updatedPosition, velocity = updatedVelocity, gun = updatedGun }

        render ship@Ship{position, gun} resources@Resources{images} = do
            drawAtPosition ship resources position

            let (_, (width, height)) = images ! (fst imageDef)
            let x = Point.x position
            let y = Point.y position
            when (x < width / 2) (drawAtPosition ship resources Point { x = Constants.nativeWidth + x, y = y })
            when (x > Constants.nativeWidth - (width / 2)) (drawAtPosition ship resources Point { x = x - Constants.nativeWidth, y = y })
            when (y < height / 2) (drawAtPosition ship resources Point { x = x, y = Constants.nativeHeight + y })
            when (y > Constants.nativeHeight - (height / 2)) (drawAtPosition ship resources Point { x = x, y = y - Constants.nativeHeight })
            
            Entity.render gun resources