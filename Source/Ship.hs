{-# LANGUAGE NamedFieldPuns #-}

module Ship
    ( Ship(Ship)
    , Ship.new
    , Ship.update'
    ) where

    import Point (Point(Point, x, y), fromAngle, clamp)
    import Constants (nativeWidth, nativeHeight)
    import Entity (EntityClass(load, update, render), Entity(Entity))
    import Resources (Resources(Resources, images), ResourceKey(ResourceKey))
    import Input (Input(Input, deltaTime, keyboard, resources))
    import Keyboard (Keyboard(Keyboard, left, right, up, down))
    import Utils (clamp, lerp, wrap, )
    import Control.Monad (when)
    import Gun (Gun, new, setCoordinates, update')
    import Sprite (Sprite(imageDef, position, rotation, isEnabled, height, renderAtPosition, dimensions))
    import Collidable (Collidable(render))

    data Ship = Ship
        { _position :: Point
        , _velocity :: Point
        , _rotation :: Double
        , _rotationVelocity :: Double
        , _gun :: Gun 
        , _isEnabled :: Bool
        }

    new :: Ship
    new = Ship
        { _position = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        , _velocity = Point { x = 0, y = 0 }
        , _rotation = 0
        , _rotationVelocity = 0
        , _gun = Gun.new
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
    update' ship@Ship{_position, _velocity, _rotation, _rotationVelocity, _gun} input@Input{deltaTime, keyboard, resources} = if Sprite.isEnabled ship
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

                gunAngle = rotation' - (pi / 2)
                images = Resources.images resources
                height = Sprite.height ship resources
                gunUnitVector = Point.fromAngle gunAngle
                gunVector = Point { x = (Point.x gunUnitVector) * (height / 2), y = (Point.y gunUnitVector) * (height / 2) }
                gunPosition = Point { x = (Point.x position') + (Point.x gunVector), y = (Point.y position') + (Point.y gunVector) }
                wrappedGunPosition = Point { x = Utils.wrap 0 Constants.nativeWidth (Point.x gunPosition), y = Utils.wrap 0 Constants.nativeHeight (Point.y gunPosition) }
                coordinatesSetGun = Gun.setCoordinates _gun wrappedGunPosition gunAngle
                gun' = Gun.update' coordinatesSetGun input
            in
                ship
                    { _rotation = rotation'
                    , _rotationVelocity = rotationVelocity'
                    , _position = position'
                    , _velocity = velocity'
                    , _gun = gun'
                    }
        else
            ship

    instance EntityClass Ship where

        load ship@Ship{_gun} = Entity.load _gun ++ [imageDef ship]

        update ship input = Entity $ Ship.update' ship input

        render ship@Ship{_position, _gun} resources@Resources{images} = do
            Collidable.render ship resources

            let (width, height) = Sprite.dimensions ship resources
            let Point{x, y} = _position

            when (x < width / 2) (Sprite.renderAtPosition ship resources Point { x = Constants.nativeWidth + x, y = y })
            when (x > Constants.nativeWidth - (width / 2)) (Sprite.renderAtPosition ship resources Point { x = x - Constants.nativeWidth, y = y })
            when (y < height / 2) (Sprite.renderAtPosition ship resources Point { x = x, y = Constants.nativeHeight + y })
            when (y > Constants.nativeHeight - (height / 2)) (Sprite.renderAtPosition ship resources Point { x = x, y = y - Constants.nativeHeight })

            Entity.render _gun resources

    instance Sprite Ship where
        imageDef _ = (ResourceKey "Ship", "Resources/Ship.png")
        position Ship{_position} = _position
        rotation Ship{_rotation} = _rotation
        isEnabled Ship{_isEnabled} = _isEnabled

    instance Collidable Ship