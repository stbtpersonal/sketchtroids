{-# LANGUAGE NamedFieldPuns #-}

module Ship
    ( Ship(Ship)
    , Ship.new
    , Ship.update'
    , Ship.explode
    ) where

    import Point (Point(Point, x, y), fromAngle, clamp)
    import Constants (nativeWidth, nativeHeight)
    import Entity (EntityClass(load, update, render), Entity(Entity))
    import Resources (Resources(Resources, images), ResourceKey(ResourceKey))
    import Input (Input(Input, deltaTime, keyboard, resources))
    import Keyboard (Keyboard(Keyboard, left, right, up, down))
    import Utils (clamp, lerp, wrap)
    import Gun (Gun, new, setCoordinates, update')
    import Sprite (Sprite(imageDefs, position, rotation, isEnabled, height, renderAtPosition, dimensions, setEnabled, isWrappingHorizontal, isWrappingVertical, spriteIndex, setSpriteIndex))
    import Collidable (Collidable(render))

    data Ship = Ship
        { _position :: Point
        , _velocity :: Point
        , _rotation :: Double
        , _rotationVelocity :: Double
        , _gun :: Gun 
        , _isEnabled :: Bool
        , _isExploding :: Bool
        , _spriteIndex :: Int
        }

    new :: Ship
    new = Ship
        { _position = Point { x = Constants.nativeWidth / 2, y = Constants.nativeHeight / 2 }
        , _velocity = Point { x = 0, y = 0 }
        , _rotation = 0
        , _rotationVelocity = 0
        , _gun = Gun.new
        , _isEnabled = False
        , _isExploding = False
        , _spriteIndex = 0
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
    update' ship@Ship{_position, _velocity, _rotation, _rotationVelocity, _gun, _isExploding} input@Input{deltaTime, keyboard, resources} = if Sprite.isEnabled ship
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

                spriteIndex' = if not _isExploding then 0 else 1
            in
                ship
                    { _rotation = rotation'
                    , _rotationVelocity = rotationVelocity'
                    , _position = position'
                    , _velocity = velocity'
                    , _gun = gun'
                    , _spriteIndex = spriteIndex'
                    }
        else
            ship

    explode :: Ship -> Ship
    explode ship = ship{_isExploding = True}

    instance EntityClass Ship where

        load ship@Ship{_gun} = Entity.load _gun ++ imageDefs ship

        update ship input = Entity $ Ship.update' ship input

        render ship@Ship{_position, _gun, _isExploding} resources@Resources{images} = do
            Collidable.render ship resources

            Entity.render _gun resources

    instance Sprite Ship where
        imageDefs _ = [(ResourceKey "Ship", "Resources/Ship.png"), (ResourceKey "ShipExplosion", "Resources/ShipExplosion.png")]
        position Ship{_position} = _position
        rotation Ship{_rotation} = _rotation
        isEnabled Ship{_isEnabled} = _isEnabled
        setEnabled ship enabled = ship{_isEnabled = enabled}
        isWrappingHorizontal _ = True
        isWrappingVertical _ = True
        spriteIndex Ship{_spriteIndex} = _spriteIndex
        setSpriteIndex ship index = ship{_spriteIndex = index}

    instance Collidable Ship