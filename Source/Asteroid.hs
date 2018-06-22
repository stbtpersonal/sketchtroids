{-# LANGUAGE NamedFieldPuns #-}

module Asteroid
    ( Asteroid()
    , Asteroid.new
    , Asteroid.imageDefs'
    , Asteroid.collisionDefs'
    , Asteroid.update'
    , Asteroid.receiveHit
    , Asteroid.break
    , Asteroid.explode
    , Asteroid.getScore
    ) where

    import Point
    import Entity
    import Resources
    import Input
    import System.Random as Random
    import Utils
    import Constants
    import Control.Monad
    import Collidable
    import Sprite
    import Explosion

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
        , _arrivingDirection = fromLeft bigAsteroidInfo
        , _hasArrived = False
        , _isEnabled = True
        , _timesHit = 0
        , _asteroidType = bigAsteroid1
        }

    imageDefs' :: [Resources.ResourceDef]
    imageDefs' =
        [ (ResourceKey "Asteroid", Image, "Resources/Asteroid.png")
        , (ResourceKey "Asteroid2", Image, "Resources/Asteroid2.png")
        , (ResourceKey "Asteroid3", Image, "Resources/Asteroid3.png")
        , (ResourceKey "AsteroidCircle", Image, "Resources/AsteroidCircle.png")
        , (ResourceKey "AsteroidRectangle", Image, "Resources/AsteroidRectangle.png")
        , (ResourceKey "AsteroidSlice", Image, "Resources/AsteroidSlice.png")
        , (ResourceKey "AsteroidSquare", Image, "Resources/AsteroidSquare.png")
        , (ResourceKey "AsteroidTriangleAcute", Image, "Resources/AsteroidTriangleAcute.png")
        , (ResourceKey "AsteroidTriangleObtuse", Image, "Resources/AsteroidTriangleObtuse.png")
        , (ResourceKey "AsteroidTriangleRight", Image, "Resources/AsteroidTriangleRight.png")
        , bigAsteroidExplosion
        , smallAsteroidExplosion
        ]

    collisionDefs' :: [Resources.ResourceDef]
    collisionDefs' =
        [ bigAsteroidCollision
        , bigAsteroidCollision
        , bigAsteroidCollision
        , (ResourceKey "AsteroidCircleCollision", Collision, "Resources/AsteroidCircleCollision.png")
        , (ResourceKey "AsteroidRectangleCollision", Collision, "Resources/AsteroidRectangleCollision.png")
        , (ResourceKey "AsteroidSliceCollision", Collision, "Resources/AsteroidSliceCollision.png")
        , (ResourceKey "AsteroidSquareCollision", Collision, "Resources/AsteroidSquareCollision.png")
        , (ResourceKey "AsteroidTriangleAcuteCollision", Collision, "Resources/AsteroidTriangleAcuteCollision.png")
        , (ResourceKey "AsteroidTriangleObtuseCollision", Collision, "Resources/AsteroidTriangleObtuseCollision.png")
        , (ResourceKey "AsteroidTriangleRightCollision", Collision, "Resources/AsteroidTriangleRightCollision.png")
        ]

    bigAsteroidCollision :: Resources.ResourceDef
    bigAsteroidCollision = (ResourceKey "AsteroidBigCollision", Collision, "Resources/AsteroidBigCollision.png")

    data AsteroidType = AsteroidType
        { _spriteIndex :: Int
        , _asteroidInfo :: AsteroidInfo
        , _breaksInto :: [AsteroidType]
        }

    bigAsteroid1 :: AsteroidType
    bigAsteroid1 = AsteroidType
        { _spriteIndex = 0
        , _asteroidInfo = bigAsteroidInfo
        , _breaksInto = [asteroidCircle, asteroidRectangle, asteroidSlice, asteroidSquare, asteroidTriangleAcute, asteroidTriangleObtuse, asteroidTriangleRight]
        }

    bigAsteroid2 :: AsteroidType
    bigAsteroid2 = AsteroidType
        { _spriteIndex = 1
        , _asteroidInfo = bigAsteroidInfo
        , _breaksInto = [asteroidCircle, asteroidRectangle, asteroidSlice, asteroidSquare, asteroidTriangleAcute, asteroidTriangleObtuse, asteroidTriangleRight]
        }

    bigAsteroid3 :: AsteroidType
    bigAsteroid3 = AsteroidType
        { _spriteIndex = 2
        , _asteroidInfo = bigAsteroidInfo
        , _breaksInto = [asteroidCircle, asteroidRectangle, asteroidSlice, asteroidSquare, asteroidTriangleAcute, asteroidTriangleObtuse, asteroidTriangleRight]
        }

    asteroidCircle :: AsteroidType
    asteroidCircle = AsteroidType
        { _spriteIndex = 3
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    asteroidRectangle :: AsteroidType
    asteroidRectangle = AsteroidType
        { _spriteIndex = 4
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    asteroidSlice :: AsteroidType
    asteroidSlice = AsteroidType
        { _spriteIndex = 5
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    asteroidSquare :: AsteroidType
    asteroidSquare = AsteroidType
        { _spriteIndex = 6
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    asteroidTriangleAcute :: AsteroidType
    asteroidTriangleAcute = AsteroidType
        { _spriteIndex = 7
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    asteroidTriangleObtuse :: AsteroidType
    asteroidTriangleObtuse = AsteroidType
        { _spriteIndex = 8
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    asteroidTriangleRight :: AsteroidType
    asteroidTriangleRight = AsteroidType
        { _spriteIndex = 9
        , _asteroidInfo = smallAsteroidInfo
        , _breaksInto = []
        }

    bigAsteroidTypes :: [AsteroidType]
    bigAsteroidTypes = [bigAsteroid1, bigAsteroid2, bigAsteroid3]

    data AsteroidInfo = AsteroidInfo
        { _minVelocity :: Double
        , _maxVelocity :: Double
        , _arrivalVelocityMultiplier :: Double
        , _maxRotationVelocity :: Double
        , _maxTimesHit :: Integer
        , _explosionImageDef :: Resources.ResourceDef
        , _explosionDuration :: Double
        , _score :: Integer
        }

    bigAsteroidInfo :: AsteroidInfo
    bigAsteroidInfo = AsteroidInfo
        { _minVelocity = 0.05
        , _maxVelocity = 0.1
        , _arrivalVelocityMultiplier = 0.5
        , _maxRotationVelocity = 0.001
        , _maxTimesHit = 5
        , _explosionImageDef = bigAsteroidExplosion
        , _explosionDuration = 1000
        , _score = 10
        }

    smallAsteroidInfo :: AsteroidInfo
    smallAsteroidInfo = AsteroidInfo
        { _minVelocity = 0.1
        , _maxVelocity = 0.2
        , _arrivalVelocityMultiplier = 1
        , _maxRotationVelocity = 0.002
        , _maxTimesHit = 2
        , _explosionImageDef = smallAsteroidExplosion
        , _explosionDuration = 1000
        , _score = 5
        }

    bigAsteroidExplosion :: Resources.ResourceDef
    bigAsteroidExplosion = (ResourceKey "ExplosionBig", Image, "Resources/ExplosionBig.png")

    smallAsteroidExplosion :: Resources.ResourceDef
    smallAsteroidExplosion = (ResourceKey "ExplosionSmall", Image, "Resources/ExplosionSmall.png")

    arrivalMargin :: Double
    arrivalMargin = 200

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

    fromLeft :: AsteroidInfo -> ArrivingDirection
    fromLeft AsteroidInfo{_minVelocity, _maxVelocity, _arrivalVelocityMultiplier} = ArrivingDirection
        { _minPositionX = -arrivalMargin
        , _maxPositionX = -arrivalMargin
        , _minPositionY = arrivalMargin
        , _maxPositionY = Constants.nativeHeight - arrivalMargin

        , _minVelocityX = _minVelocity
        , _maxVelocityX = _maxVelocity
        , _minVelocityY = -_maxVelocity * _arrivalVelocityMultiplier
        , _maxVelocityY = _maxVelocity * _arrivalVelocityMultiplier

        , _isInBounds = isInBoundsFromLeft
        , _orientation = Horizontal
        }

    isInBoundsFromLeft :: Point.Point -> Bool
    isInBoundsFromLeft Point{x} = x > arrivalMargin 

    fromRight :: AsteroidInfo -> ArrivingDirection
    fromRight AsteroidInfo{_minVelocity, _maxVelocity, _arrivalVelocityMultiplier} = ArrivingDirection
        { _minPositionX = Constants.nativeWidth + arrivalMargin
        , _maxPositionX = Constants.nativeWidth + arrivalMargin
        , _minPositionY = arrivalMargin
        , _maxPositionY = Constants.nativeHeight - arrivalMargin

        , _minVelocityX = -_maxVelocity
        , _maxVelocityX = -_minVelocity
        , _minVelocityY = -_maxVelocity * _arrivalVelocityMultiplier
        , _maxVelocityY = _maxVelocity * _arrivalVelocityMultiplier

        , _isInBounds = isInBoundsFromRight
        , _orientation = Horizontal
        }

    isInBoundsFromRight :: Point.Point -> Bool
    isInBoundsFromRight Point{x} = x < Constants.nativeWidth - arrivalMargin

    fromTop :: AsteroidInfo -> ArrivingDirection
    fromTop AsteroidInfo{_minVelocity, _maxVelocity, _arrivalVelocityMultiplier} = ArrivingDirection
        { _minPositionX = arrivalMargin
        , _maxPositionX = Constants.nativeWidth - arrivalMargin
        , _minPositionY = -arrivalMargin
        , _maxPositionY = -arrivalMargin

        , _minVelocityX = -_maxVelocity * _arrivalVelocityMultiplier
        , _maxVelocityX = _maxVelocity * _arrivalVelocityMultiplier
        , _minVelocityY = _minVelocity
        , _maxVelocityY = _maxVelocity

        , _isInBounds = isInBoundsFromTop
        , _orientation = Vertical
        }

    isInBoundsFromTop :: Point.Point -> Bool
    isInBoundsFromTop Point{y} = y > arrivalMargin

    fromBottom :: AsteroidInfo -> ArrivingDirection
    fromBottom AsteroidInfo{_minVelocity, _maxVelocity, _arrivalVelocityMultiplier} = ArrivingDirection 
        { _minPositionX = arrivalMargin
        , _maxPositionX = Constants.nativeWidth - arrivalMargin
        , _minPositionY = Constants.nativeHeight + arrivalMargin
        , _maxPositionY = Constants.nativeHeight + arrivalMargin

        , _minVelocityX = -_maxVelocity * _arrivalVelocityMultiplier
        , _maxVelocityX = _maxVelocity * _arrivalVelocityMultiplier
        , _minVelocityY = -_maxVelocity
        , _maxVelocityY = -_minVelocity

        , _isInBounds = isInBoundsFromBottom
        , _orientation = Vertical
        }

    isInBoundsFromBottom :: Point.Point -> Bool
    isInBoundsFromBottom Point{y} = y < Constants.nativeHeight - arrivalMargin

    arrivingDirections :: AsteroidInfo -> [ArrivingDirection]
    arrivingDirections asteroidInfo = map (\func -> func asteroidInfo) [fromLeft, fromRight, fromTop, fromBottom]

    receiveHit :: Asteroid -> Asteroid
    receiveHit asteroid@Asteroid{_timesHit, _asteroidType} = 
        let
            timesHit' = _timesHit + 1

            AsteroidType{_asteroidInfo} = _asteroidType
            AsteroidInfo{_maxTimesHit} = _asteroidInfo
            asteroid' = if timesHit' < _maxTimesHit
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
                        if not _isInitialized then initializeRandomAsteroid asteroid input else asteroid
                    
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

    initializeRandomAsteroid :: Asteroid -> Input -> Asteroid
    initializeRandomAsteroid asteroid input@Input{randomGenerator} =
        let
            (bigAsteroidTypeIndex, randomGenerator1) = Random.randomR (0, length bigAsteroidTypes - 1) randomGenerator
            asteroidType@AsteroidType{_asteroidInfo} = bigAsteroidTypes !! bigAsteroidTypeIndex
        in
            initializeAsteroid asteroid asteroidType $ Input.randomizeNext input

    initializeAsteroid :: Asteroid -> AsteroidType -> Input -> Asteroid
    initializeAsteroid asteroid asteroidType@AsteroidType{_asteroidInfo} Input{randomGenerator} =
        let
            arrivingDirections' = arrivingDirections _asteroidInfo
            (arrivingDirectionIndex, randomGenerator1) = Random.randomR (0, length arrivingDirections' - 1) randomGenerator
            arrivingDirection' = arrivingDirections' !! arrivingDirectionIndex
            (positionX, randomGenerator2) = Random.randomR (_minPositionX arrivingDirection', _maxPositionX arrivingDirection') randomGenerator1
            (positionY, randomGenerator3) = Random.randomR (_minPositionY arrivingDirection', _maxPositionY arrivingDirection') randomGenerator2
            (velocityX, randomGenerator4) = Random.randomR (_minVelocityX arrivingDirection', _maxVelocityX arrivingDirection') randomGenerator3
            (velocityY, randomGenerator5) = Random.randomR (_minVelocityY arrivingDirection', _maxVelocityY arrivingDirection') randomGenerator4

            AsteroidInfo{_maxRotationVelocity} = _asteroidInfo
            (rotationVelocity, _) = Random.randomR (-_maxRotationVelocity, _maxRotationVelocity) randomGenerator5
        in
            asteroid
                { _position = Point { x = positionX, y = positionY }
                , _velocity = Point { x = velocityX, y = velocityY }
                , _rotationVelocity = rotationVelocity
                , _arrivingDirection = arrivingDirection'
                , _isInitialized = True
                , _asteroidType = asteroidType
                }

    break :: Asteroid -> Input -> [Asteroid]
    break Asteroid{_position, _asteroidType} input = 
        let
            fragmentTypes = _breaksInto _asteroidType
            fragments = take (length fragmentTypes) $ repeat Asteroid.new
            randomizedInputs = Input.randomize input
            initializedFragments = zipWith3 (\fragment fragmentType randomizedInput -> initializeAsteroid fragment fragmentType randomizedInput) fragments fragmentTypes randomizedInputs
            initializedFragments' = map (\fragment -> fragment{_position = _position, _hasArrived = True}) initializedFragments
        in
            initializedFragments'

    explode :: Asteroid -> Input -> Explosion
    explode Asteroid{_position, _asteroidType} input = 
        let
            AsteroidType{_asteroidInfo} = _asteroidType
            AsteroidInfo{_explosionImageDef, _explosionDuration} = _asteroidInfo
        in
            Explosion.new _position _explosionImageDef _explosionDuration input

    getScore :: Asteroid -> Integer
    getScore Asteroid{_asteroidType} = 
        let
            AsteroidType{_asteroidInfo} = _asteroidType
            AsteroidInfo{_score} = _asteroidInfo
        in
            _score

    instance EntityClass Asteroid where
        load asteroid = Sprite.imageDefs asteroid ++ Collidable.collisionDefs asteroid
        update asteroid input = Entity $ update' asteroid input
        render asteroid@Asteroid{_position, _isInitialized, _hasArrived, _arrivingDirection} input = when _isInitialized $ Collidable.render asteroid input

    instance Sprite Asteroid where
        imageDefs _ = imageDefs'
        position Asteroid{_position} = _position
        setPosition asteroid position = asteroid{_position = position}
        rotation Asteroid{_rotation} = _rotation
        isEnabled Asteroid{_isEnabled} = _isEnabled
        setEnabled asteroid enabled = asteroid{_isEnabled = enabled}
        isWrappingHorizontal Asteroid{_hasArrived, _arrivingDirection} = _hasArrived || _orientation _arrivingDirection == Vertical
        isWrappingVertical Asteroid{_hasArrived, _arrivingDirection} = _hasArrived || _orientation _arrivingDirection == Horizontal
        spriteIndex Asteroid{_asteroidType} = _spriteIndex _asteroidType

    instance Collidable Asteroid where
        collisionDefs _ = collisionDefs'

    instance Eq Asteroid where
        left@Asteroid{_position = positionLeft, _rotation = rotationLeft} == right@Asteroid{_position = positionRight, _rotation = rotationRight} =
            positionLeft == positionRight && rotationLeft == rotationRight