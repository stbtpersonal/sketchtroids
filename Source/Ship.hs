{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Ship
    ( Ship(Ship)
    , Ship.new
    , Ship.update'
    , Ship.explode
    , Ship.updateGun
    ) where

    import Point
    import Constants
    import Entity
    import Resources
    import Input
    import Keyboard
    import Utils
    import Gun
    import Sprite
    import Collidable
    import Explosion
    import Resources

    data Ship = Ship
        { _position :: Point
        , _velocity :: Point
        , _rotation :: Double
        , _rotationVelocity :: Double
        , _isEnabled :: Bool
        }

    new :: Ship
    new = Ship
        { _position = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        , _velocity = Point { x = 0, y = 0 }
        , _rotation = 0
        , _rotationVelocity = 0
        , _isEnabled = False
        }

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
    update' ship@Ship{_position, _velocity, _rotation, _rotationVelocity} input@Input{deltaTime, keyboard, resources} = if Sprite.isEnabled ship
        then
            let
                leftValue = getValue Keyboard.left keyboard deltaTime rotationAcceleration
                rightValue = getValue Keyboard.right keyboard deltaTime rotationAcceleration
                upValue = getValue Keyboard.up keyboard deltaTime accelerationForward
                downValue = getValue Keyboard.down keyboard deltaTime accelerationBackward
                rotationValueDelta = rightValue - leftValue
                positionValueDelta = downValue - upValue

                rotation' = _rotation + (_rotationVelocity * deltaTime)
                rotationVelocity' = Utils.clamp (-maxRotationVelocity) maxRotationVelocity (Utils.lerp (_rotationVelocity + rotationValueDelta) 0 rotationDecelerationLerp)

                nextX = (Point.x _position) + (Point.x _velocity * deltaTime)
                nextY = (Point.y _position) + (Point.y _velocity * deltaTime)
                position' = Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }
                fowardAngle = rotation' + (pi / 2)
                accelerationX = positionValueDelta * cos fowardAngle
                accelerationY = positionValueDelta * sin fowardAngle
                nextVelocity = Point { x = (Point.x _velocity) + accelerationX, y = (Point.y _velocity) + accelerationY }
                velocity' = Point.clamp maxVelocityBackward maxVelocityForward nextVelocity
            in
                ship
                    { _rotation = rotation'
                    , _rotationVelocity = rotationVelocity'
                    , _position = position'
                    , _velocity = velocity'
                    }
        else
            ship

    updateGun :: Gun -> Ship -> Input -> Gun
    updateGun gun ship@Ship{_position, _rotation} input@Input{resources} = if Sprite.isEnabled ship
        then
            let
                gunAngle = _rotation - (pi / 2)
                height = Sprite.height ship resources
                gunUnitVector = Point.fromAngle gunAngle
                gunVector = Point { x = (Point.x gunUnitVector) * (height / 2), y = (Point.y gunUnitVector) * (height / 2) }
                gunPosition = Point { x = (Point.x _position) + (Point.x gunVector), y = (Point.y _position) + (Point.y gunVector) }
                wrappedGunPosition = Point { x = Utils.wrap 0 Constants.nativeWidth (Point.x gunPosition), y = Utils.wrap 0 Constants.nativeHeight (Point.y gunPosition) }
                !coordinatesSetGun = Gun.setCoordinates gun wrappedGunPosition gunAngle
                gun' = Gun.update' coordinatesSetGun input
            in
                gun'
        else
            gun

    explosionImageDef :: Resources.ResourceDef
    explosionImageDef = (ResourceKey "ShipExplosion", "Resources/ShipExplosion.png")

    explosionDuration :: Double
    explosionDuration = 1000

    explode :: Ship -> Explosion
    explode Ship{_position} = Explosion.new _position explosionImageDef explosionDuration

    instance EntityClass Ship where
        load ship = imageDefs ship
        update ship input = Entity $ Ship.update' ship input
        render ship input = Collidable.render ship input

    instance Sprite Ship where
        imageDefs _ = [(ResourceKey "Ship", "Resources/Ship.png"), explosionImageDef]
        position Ship{_position} = _position
        setPosition ship position = ship{_position = position}
        rotation Ship{_rotation} = _rotation
        isEnabled Ship{_isEnabled} = _isEnabled
        setEnabled ship enabled = ship{_isEnabled = enabled}
        isWrappingHorizontal _ = True
        isWrappingVertical _ = True

    instance Collidable Ship