{-# LANGUAGE NamedFieldPuns #-}

module Gun
    ( Gun()
    , Gun.new
    , Gun.update'
    , setCoordinates
    ) where

    import Bullet
    import Input
    import Entity
    import Point
    import Resources
    import Constants
    import Keyboard
    import Data.Map
    import Sprite

    data Gun = Gun
        { _bullets :: [Bullet]
        , _timeCount :: Double
        , _lastFiredTime :: Double
        , _position :: Point.Point
        , _rotation :: Double
        , _isEnabled :: Bool
        }

    maxBullets :: Int
    maxBullets = 50

    new :: Gun
    new = Gun 
        { _bullets = []
        , _timeCount = 0
        , _lastFiredTime = 0
        , _position = Point { x = 800, y = 800 }
        , _rotation = 0
        , _isEnabled = False
        }

    intervalBetweenFiring :: Double
    intervalBetweenFiring = 300.0

    setCoordinates :: Gun -> Point -> Double -> Gun
    setCoordinates gun position rotation = gun { _position = position, _rotation = rotation }

    update' :: Gun -> Input -> Gun
    update' gun@Gun{_bullets, _timeCount, _lastFiredTime, _position, _rotation, _isEnabled} input@Input{keyboard, deltaTime, resources} =
        let
            timeCount' = _timeCount + deltaTime
            bullets' = Prelude.map (\bullet -> Bullet.update' bullet input) _bullets

            isFiring = Keyboard.action keyboard && timeCount' - _lastFiredTime > intervalBetweenFiring && _isEnabled
            lastFiredTime' = if isFiring then timeCount' else _lastFiredTime

            bullets'' = if isFiring
                then (Bullet.new _position $ Point.fromAngle _rotation) : bullets'
                else bullets'

            bullets''' = Prelude.filter (\bullet -> not $ Bullet.isOutOfBounds bullet resources) bullets''
        in
            gun
                { _bullets = bullets'''
                , _timeCount = timeCount'
                , _lastFiredTime = lastFiredTime'
                }

    instance EntityClass Gun where
        load gun = Sprite.imageDefs gun
        update gun input = Entity $ Gun.update' gun input
        render Gun{_bullets} resources = Entity.renderAll (Prelude.map Entity _bullets) resources

    instance Sprite Gun where
        imageDef _ = Bullet.imageDef'
        position Gun{_position} = _position
        rotation Gun{_rotation} = _rotation
        isEnabled Gun{_isEnabled} = _isEnabled
        setEnabled gun enabled = gun{_isEnabled = enabled}