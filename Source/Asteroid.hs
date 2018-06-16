{-# LANGUAGE NamedFieldPuns #-}

module Asteroid
    ( Asteroid()
    , Asteroid.new
    , Asteroid.imageDefs'
    , Asteroid.update'
    , Asteroid.receiveHit
    ) where

    import Point (Point(Point, x, y))
    import Entity (EntityClass(load, update, render), Entity(Entity))
    import Resources (Resources(Resources, images), ResourceKey(ResourceKey), ResourceDef)
    import Input (Input(Input, deltaTime, randomGenerator))
    import System.Random as Random (randomR)
    import Utils (wrap)
    import Constants (nativeWidth, nativeHeight)
    import Control.Monad (when)
    import Collidable (Collidable, render)
    import Sprite (Sprite(imageDef, imageDefs, position, rotation, render, renderAtPosition, dimensions, isEnabled, setEnabled, isWrappingHorizontal, isWrappingVertical, spriteIndex, setSpriteIndex))

    data Asteroid = Asteroid
        { _position :: Point
        , _velocity :: Point
        , _rotation :: Double
        , _rotationVelocity :: Double
        , _isInitialized :: Bool
        , _arrivingDirection :: ArrivingDirection
        , _hasArrived :: Bool
        , _isEnabled :: Bool
        , _timesHit :: Integer
        , _spriteIndex :: Int
        }

    new :: Asteroid
    new = Asteroid
        { _position = Point { x = 0, y = 0 }
        , _velocity = Point { x = 0, y = 0 }
        , _rotation = 0
        , _rotationVelocity = 0
        , _isInitialized = False
        , _arrivingDirection = fromLeft
        , _hasArrived = False
        , _isEnabled = True
        , _timesHit = 0
        , _spriteIndex = 0
        }

    imageDefs' :: [Resources.ResourceDef]
    imageDefs' = [(ResourceKey "Asteroid", "Resources/Asteroid.png"), (ResourceKey "Asteroid2", "Resources/Asteroid2.png"), (ResourceKey "Asteroid3", "Resources/Asteroid3.png")]

    minVelocity :: Double
    minVelocity = 0.05

    maxVelocity :: Double
    maxVelocity = 0.1

    maxRotationVelocity :: Double
    maxRotationVelocity = 0.001

    arrivalMargin :: Double
    arrivalMargin = 200

    arrivalVelocityMultiplier :: Double
    arrivalVelocityMultiplier = 0.5

    data ArrivingDirection = ArrivingDirection
        { _minPositionX :: Double
        , _maxPositionX :: Double
        , _minPositionY :: Double
        , _maxPositionY :: Double 

        , _minVelocityX :: Double
        , _maxVelocityX :: Double
        , _minVelocityY :: Double
        , _maxVelocityY :: Double

        , _isInBounds :: Point.Point -> Bool
        , _orientation :: Orientation
        }

    data Orientation = Horizontal | Vertical deriving Eq

    fromLeft :: ArrivingDirection
    fromLeft = ArrivingDirection
        { _minPositionX = -arrivalMargin
        , _maxPositionX = -arrivalMargin
        , _minPositionY = arrivalMargin
        , _maxPositionY = Constants.nativeHeight - arrivalMargin

        , _minVelocityX = minVelocity
        , _maxVelocityX = maxVelocity
        , _minVelocityY = -maxVelocity * arrivalVelocityMultiplier
        , _maxVelocityY = maxVelocity * arrivalVelocityMultiplier

        , _isInBounds = isInBoundsFromLeft
        , _orientation = Horizontal
        }

    isInBoundsFromLeft :: Point.Point -> Bool
    isInBoundsFromLeft Point{x} = x > arrivalMargin 

    fromRight :: ArrivingDirection
    fromRight = ArrivingDirection
        { _minPositionX = Constants.nativeWidth + arrivalMargin
        , _maxPositionX = Constants.nativeWidth + arrivalMargin
        , _minPositionY = arrivalMargin
        , _maxPositionY = Constants.nativeHeight - arrivalMargin

        , _minVelocityX = -maxVelocity
        , _maxVelocityX = -minVelocity
        , _minVelocityY = -maxVelocity * arrivalVelocityMultiplier
        , _maxVelocityY = maxVelocity * arrivalVelocityMultiplier

        , _isInBounds = isInBoundsFromRight
        , _orientation = Horizontal
        }

    isInBoundsFromRight :: Point.Point -> Bool
    isInBoundsFromRight Point{x} = x < Constants.nativeWidth - arrivalMargin

    fromTop :: ArrivingDirection
    fromTop = ArrivingDirection
        { _minPositionX = arrivalMargin
        , _maxPositionX = Constants.nativeWidth - arrivalMargin
        , _minPositionY = -arrivalMargin
        , _maxPositionY = -arrivalMargin

        , _minVelocityX = -maxVelocity * arrivalVelocityMultiplier
        , _maxVelocityX = maxVelocity * arrivalVelocityMultiplier
        , _minVelocityY = minVelocity
        , _maxVelocityY = maxVelocity

        , _isInBounds = isInBoundsFromTop
        , _orientation = Vertical
        }

    isInBoundsFromTop :: Point.Point -> Bool
    isInBoundsFromTop Point{y} = y > arrivalMargin

    fromBottom :: ArrivingDirection
    fromBottom = ArrivingDirection 
        { _minPositionX = arrivalMargin
        , _maxPositionX = Constants.nativeWidth - arrivalMargin
        , _minPositionY = Constants.nativeHeight + arrivalMargin
        , _maxPositionY = Constants.nativeHeight + arrivalMargin

        , _minVelocityX = -maxVelocity * arrivalVelocityMultiplier
        , _maxVelocityX = maxVelocity * arrivalVelocityMultiplier
        , _minVelocityY = -maxVelocity
        , _maxVelocityY = -minVelocity

        , _isInBounds = isInBoundsFromBottom
        , _orientation = Vertical
        }

    isInBoundsFromBottom :: Point.Point -> Bool
    isInBoundsFromBottom Point{y} = y < Constants.nativeHeight - arrivalMargin

    arrivingDirections :: [ArrivingDirection]
    arrivingDirections = [fromLeft, fromRight, fromTop, fromBottom]

    maxTimesHit :: Integer
    maxTimesHit = 5

    receiveHit :: Asteroid -> Asteroid
    receiveHit asteroid@Asteroid{_timesHit} = 
        let
            timesHit' = _timesHit + 1
            asteroid' = if timesHit' < 5
                then asteroid{_timesHit = timesHit'}
                else asteroid{_timesHit = timesHit', _isEnabled = False}
        in 
            asteroid'
            

    update' :: Asteroid -> Input -> Asteroid
    update' asteroid@Asteroid{_position, _velocity, _rotation, _rotationVelocity, _isInitialized, _hasArrived, _arrivingDirection, _spriteIndex} Input{deltaTime, randomGenerator} =
        if Sprite.isEnabled asteroid
            then
                let
                    (position', velocity', rotationVelocity', arrivingDirection', spriteIndex') = if not _isInitialized
                        then
                            let
                                (arrivingDirectionIndex, randomGenerator1) = Random.randomR (0, length arrivingDirections - 1) randomGenerator
                                arrivingDirection = arrivingDirections !! arrivingDirectionIndex
                                (positionX, randomGenerator2) = Random.randomR (_minPositionX arrivingDirection, _maxPositionX arrivingDirection) randomGenerator1
                                (positionY, randomGenerator3) = Random.randomR (_minPositionY arrivingDirection, _maxPositionY arrivingDirection) randomGenerator2
                                (velocityX, randomGenerator4) = Random.randomR (_minVelocityX arrivingDirection, _maxVelocityX arrivingDirection) randomGenerator3
                                (velocityY, randomGenerator5) = Random.randomR (_minVelocityY arrivingDirection, _maxVelocityY arrivingDirection) randomGenerator4
                                (rotationVelocity, randomGenerator6) = Random.randomR (-maxRotationVelocity, maxRotationVelocity) randomGenerator5
                                (spriteIndex, _) = Random.randomR (0, length imageDefs' - 1) randomGenerator6
                            in
                                (Point { x = positionX, y = positionY }, Point { x = velocityX, y = velocityY }, rotationVelocity, arrivingDirection, spriteIndex)
                        else
                            (_position, _velocity, _rotationVelocity, _arrivingDirection, _spriteIndex)

                    hasArrived' = _hasArrived || (_isInBounds arrivingDirection' $ position')

                    nextX = (Point.x position') + (Point.x velocity' * deltaTime)
                    nextY = (Point.y position') + (Point.y velocity' * deltaTime)
                    position'' = if not hasArrived'
                        then Point { x = nextX, y = nextY }
                        else Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }

                    rotation' = _rotation + (rotationVelocity' * deltaTime)
                in
                    asteroid
                        { _position = position''
                        , _velocity = velocity'
                        , _rotation = rotation'
                        , _rotationVelocity = rotationVelocity'
                        , _isInitialized = True
                        , _arrivingDirection = arrivingDirection' 
                        , _hasArrived = hasArrived'
                        , _spriteIndex = spriteIndex'
                        }
            else
                asteroid

    instance EntityClass Asteroid where
        load asteroid = Sprite.imageDefs asteroid
        update asteroid input = Entity $ update' asteroid input
        render asteroid@Asteroid{_position, _isInitialized, _hasArrived, _arrivingDirection} resources@Resources{images} =
            when _isInitialized $ Collidable.render asteroid resources

    instance Sprite Asteroid where
        imageDefs _ = imageDefs'
        position Asteroid{_position} = _position
        rotation Asteroid{_rotation} = _rotation
        isEnabled Asteroid{_isEnabled} = _isEnabled
        setEnabled asteroid enabled = asteroid{_isEnabled = enabled}
        isWrappingHorizontal Asteroid{_hasArrived, _arrivingDirection} = _hasArrived || _orientation _arrivingDirection == Vertical
        isWrappingVertical Asteroid{_hasArrived, _arrivingDirection} = _hasArrived || _orientation _arrivingDirection == Horizontal
        spriteIndex Asteroid{_spriteIndex} = _spriteIndex
        setSpriteIndex asteroid index = asteroid{_spriteIndex = index}

    instance Collidable Asteroid

    instance Eq Asteroid where
        left@Asteroid{_position = positionLeft, _rotation = rotationLeft} == right@Asteroid{_position = positionRight, _rotation = rotationRight} =
            positionLeft == positionRight && rotationLeft == rotationRight