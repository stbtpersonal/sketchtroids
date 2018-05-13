{-# LANGUAGE NamedFieldPuns #-}

module Asteroid
    ( Asteroid()
    , Asteroid.new
    , Asteroid.update'
    ) where

    import Point (Point(Point, x, y))
    import Entity (EntityClass(load, update, render), Entity(Entity))
    import Resources (Resources(Resources, images), ResourceKey(ResourceKey))
    import Input (Input(Input, deltaTime, randomGenerator))
    import System.Random as Random (randomR)
    import Utils (wrap)
    import Constants (nativeWidth, nativeHeight)
    import Control.Monad (when)
    import Collidable (Collidable, render)
    import Sprite (Sprite(imageDef, position, rotation, render, renderAtPosition, dimensions))

    data Asteroid = Asteroid
        { _position :: Point
        , _velocity :: Point
        , _rotation :: Double
        , _rotationVelocity :: Double
        , _isInitialized :: Bool
        , _arrivingDirection :: ArrivingDirection
        , _hasArrived :: Bool
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
        }

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

    update' :: Asteroid -> Input -> Asteroid
    update' asteroid@Asteroid{_position, _velocity, _rotation, _rotationVelocity, _isInitialized, _hasArrived, _arrivingDirection} Input{deltaTime, randomGenerator} =
        let
            (position', velocity', rotationVelocity', arrivingDirection') = if not _isInitialized
                then
                    let
                        (arrivingDirectionIndex, randomGenerator1) = Random.randomR (0, length arrivingDirections - 1) randomGenerator
                        arrivingDirection = arrivingDirections !! arrivingDirectionIndex
                        (positionX, randomGenerator2) = Random.randomR (_minPositionX arrivingDirection, _maxPositionX arrivingDirection) randomGenerator1
                        (positionY, randomGenerator3) = Random.randomR (_minPositionY arrivingDirection, _maxPositionY arrivingDirection) randomGenerator2
                        (velocityX, randomGenerator4) = Random.randomR (_minVelocityX arrivingDirection, _maxVelocityX arrivingDirection) randomGenerator3
                        (velocityY, randomGenerator5) = Random.randomR (_minVelocityY arrivingDirection, _maxVelocityY arrivingDirection) randomGenerator4
                        (rotationVelocity, _) = Random.randomR (-maxRotationVelocity, maxRotationVelocity) randomGenerator5
                    in
                        (Point { x = positionX, y = positionY }, Point { x = velocityX, y = velocityY }, rotationVelocity, arrivingDirection)
                else
                    (_position, _velocity, _rotationVelocity, _arrivingDirection)

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
                }

    instance EntityClass Asteroid where

        load asteroid = [Sprite.imageDef asteroid]

        update asteroid input = Entity $ update' asteroid input

        render asteroid@Asteroid{_position, _isInitialized, _hasArrived, _arrivingDirection} resources@Resources{images} = when _isInitialized $ do
            Collidable.render asteroid resources

            let isVertical = _orientation _arrivingDirection == Vertical
            let (width, height) = Sprite.dimensions asteroid resources
            let Point{x, y} = _position

            when
                ((_hasArrived || isVertical) && x < width / 2)
                (Sprite.renderAtPosition asteroid resources Point { x = Constants.nativeWidth + x, y = y })
            when 
                ((_hasArrived || isVertical) && x > Constants.nativeWidth - (width / 2))
                (Sprite.renderAtPosition asteroid resources Point { x = x - Constants.nativeWidth, y = y })
            when 
                ((_hasArrived || not isVertical) && y < height / 2)
                (Sprite.renderAtPosition asteroid resources Point { x = x, y = Constants.nativeHeight + y })
            when 
                ((_hasArrived || not isVertical) && y > Constants.nativeHeight - (height / 2)) 
                (Sprite.renderAtPosition asteroid resources Point { x = x, y = y - Constants.nativeHeight })


    instance Sprite Asteroid where
        imageDef _ = (ResourceKey "Asteroid", "Resources/Asteroid.png")
        position Asteroid{_position} = _position
        rotation Asteroid{_rotation} = _rotation

    instance Collidable Asteroid