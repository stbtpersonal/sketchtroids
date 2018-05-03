{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Ship(
    Ship(Ship),
    new,
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

    data Ship = Ship { position :: Point.Point, velocity :: Point.Point, rotation :: Double, rotationVelocity :: Double }

    new :: Ship
    new = Ship { position = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
               , velocity = Point { x = 0, y = 0 }
               , rotation = 0
               , rotationVelocity = 0
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

    instance EntityClass Ship where

        load _ = [imageDef]

        update ship@Ship{position, velocity, rotation, rotationVelocity} input@Input{deltaTime, keyboard} =
            let
                leftValue = getValue Keyboard.left keyboard deltaTime rotationAcceleration
                rightValue = getValue Keyboard.right keyboard deltaTime rotationAcceleration
                upValue = getValue Keyboard.up keyboard deltaTime accelerationForward
                downValue = getValue Keyboard.down keyboard deltaTime accelerationBackward
                rotationValueDelta = rightValue - leftValue
                positionValueDelta = downValue - upValue

                updatedRotation = rotation + (rotationVelocity * deltaTime)
                updatedRotationVelocity = Utils.clamp (-maxRotationVelocity) maxRotationVelocity (Utils.lerp (rotationVelocity + rotationValueDelta) 0 rotationDecelerationLerp)

                updatedPosition = Point { x = (Point.x position) + (Point.x velocity * deltaTime), y = (Point.y position) + (Point.y velocity * deltaTime) }
                fowardAngle = updatedRotation + (pi / 2)
                accelerationX = positionValueDelta * cos fowardAngle
                accelerationY = positionValueDelta * sin fowardAngle
                nextVelocity = Point { x = (Point.x velocity) + accelerationX, y = (Point.y velocity) + accelerationY }
                updatedVelocity = Point.clamp maxVelocityBackward maxVelocityForward nextVelocity
            in
                Entity $ ship { rotation = updatedRotation, rotationVelocity = updatedRotationVelocity, position = updatedPosition, velocity = updatedVelocity }

        render ship@Ship{position, rotation} Resources{images} =
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                translatedSprite = Canvas.translate (Point.x position, Point.y position) rotatedSprite
            in
                translatedSprite