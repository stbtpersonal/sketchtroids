{-# LANGUAGE NamedFieldPuns #-}

module Asteroid
    ( Asteroid()
    , Asteroid.new
    , Asteroid.imageDefs'
    , Asteroid.update'
    , Asteroid.receiveHit
    , Asteroid.break
    ) where

    import Point (Point(Point, x, y))
    import Entity (EntityClass(load, update, render), Entity(Entity))
    import Resources (Resources(Resources, images), ResourceKey(ResourceKey), ResourceDef)
    import Input (Input(Input, deltaTime, randomGenerator), randomize)
    import System.Random as Random (randomR)
    import Utils (wrap)
    import Constants (nativeWidth, nativeHeight)
    import Control.Monad (when)
    import Collidable (Collidable, render)
    import Sprite (Sprite(imageDef, imageDefs, position, rotation, render, renderAtPosition, dimensions, isEnabled, setEnabled, isWrappingHorizontal, isWrappingVertical, spriteIndex))

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
        , _asteroidType :: AsteroidType
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
        , _asteroidType = asteroid1
        }

    imageDefs' :: [Resources.ResourceDef]
    imageDefs' =
        [ (ResourceKey "Asteroid", "Resources/Asteroid.png")
        , (ResourceKey "Asteroid2", "Resources/Asteroid2.png")
        , (ResourceKey "Asteroid3", "Resources/Asteroid3.png")
        , (ResourceKey "AsteroidSmall", "Resources/AsteroidSmall.png")
        ]

    data AsteroidType = AsteroidType
        { _spriteIndex :: Int
        , _breaksInto :: [AsteroidType]
        }

    asteroid1 :: AsteroidType
    asteroid1 = AsteroidType
        {_spriteIndex = 0
        , _breaksInto = [asteroidSmall1, asteroidSmall1, asteroidSmall1]
        }

    asteroid2 :: AsteroidType
    asteroid2 = AsteroidType
        { _spriteIndex = 1
        , _breaksInto = [asteroidSmall1, asteroidSmall1]
        }

    asteroid3 :: AsteroidType
    asteroid3 = AsteroidType
        { _spriteIndex = 2
        , _breaksInto = [asteroidSmall1]
        }

    asteroidSmall1 :: AsteroidType
    asteroidSmall1 = AsteroidType
        { _spriteIndex = 3
        , _breaksInto = []
        }

    bigAsteroidTypes :: [AsteroidType]
    bigAsteroidTypes = [asteroid1, asteroid2, asteroid3]

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
    update' asteroid@Asteroid{_rotation, _isInitialized} input@Input{deltaTime} =
        if Sprite.isEnabled asteroid
            then
                let
                    asteroid'@Asteroid{_position = position', _velocity = velocity', _rotationVelocity = rotationVelocity', _hasArrived = hasArrived', _arrivingDirection = arrivingDirection'} =
                        if not _isInitialized then initializeAsteroid asteroid input else asteroid
                    
                    hasArrived'' = hasArrived' || (_isInBounds arrivingDirection' $ position')

                    nextX = (Point.x position') + (Point.x velocity' * deltaTime)
                    nextY = (Point.y position') + (Point.y velocity' * deltaTime)
                    position'' = if not hasArrived''
                        then Point { x = nextX, y = nextY }
                        else Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }

                    rotation' = _rotation + (rotationVelocity' * deltaTime)
                in
                    asteroid'
                        { _position = position''
                        , _rotation = rotation'
                        , _hasArrived = hasArrived''
                        }
            else
                asteroid

    initializeAsteroid :: Asteroid -> Input -> Asteroid
    initializeAsteroid asteroid Input{randomGenerator} =
        let
            (arrivingDirectionIndex, randomGenerator1) = Random.randomR (0, length arrivingDirections - 1) randomGenerator
            arrivingDirection = arrivingDirections !! arrivingDirectionIndex
            (positionX, randomGenerator2) = Random.randomR (_minPositionX arrivingDirection, _maxPositionX arrivingDirection) randomGenerator1
            (positionY, randomGenerator3) = Random.randomR (_minPositionY arrivingDirection, _maxPositionY arrivingDirection) randomGenerator2
            (velocityX, randomGenerator4) = Random.randomR (_minVelocityX arrivingDirection, _maxVelocityX arrivingDirection) randomGenerator3
            (velocityY, randomGenerator5) = Random.randomR (_minVelocityY arrivingDirection, _maxVelocityY arrivingDirection) randomGenerator4
            (rotationVelocity, randomGenerator6) = Random.randomR (-maxRotationVelocity, maxRotationVelocity) randomGenerator5
            (bigAsteroidTypeIndex, _) = Random.randomR (0, length bigAsteroidTypes - 1) randomGenerator6
            asteroidType = bigAsteroidTypes !! bigAsteroidTypeIndex
        in
            asteroid
                { _position = Point { x = positionX, y = positionY }
                , _velocity = Point { x = velocityX, y = velocityY }
                , _rotationVelocity = rotationVelocity
                , _arrivingDirection = arrivingDirection
                , _isInitialized = True
                , _asteroidType = asteroidType
                }

    break :: Asteroid -> Input -> [Asteroid]
    break Asteroid{_position, _asteroidType} input = 
        let
            fragmentTypes = _breaksInto _asteroidType
            fragments = take (length fragmentTypes) $ repeat Asteroid.new
            randomizedInputs = Input.randomize input
            initializedFragments = zipWith (\fragment randomizedInput -> initializeAsteroid fragment randomizedInput) fragments randomizedInputs
            initializedFragments' = zipWith (\fragment fragmentType -> fragment{_asteroidType = fragmentType}) initializedFragments fragmentTypes
            initializedFragments'' = map (\fragment -> fragment{_position = _position, _hasArrived = True}) initializedFragments'
        in
            initializedFragments''

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
        spriteIndex Asteroid{_asteroidType} = _spriteIndex _asteroidType

    instance Collidable Asteroid

    instance Eq Asteroid where
        left@Asteroid{_position = positionLeft, _rotation = rotationLeft} == right@Asteroid{_position = positionRight, _rotation = rotationRight} =
            positionLeft == positionRight && rotationLeft == rotationRight