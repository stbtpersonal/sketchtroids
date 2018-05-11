{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Asteroid
    ( Asteroid()
    , new
    , update'
    ) where

    import Point
    import Entity
    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas
    import System.Random as Random
    import Utils
    import Constants
    import Control.Monad

    data Asteroid = Asteroid
        { position :: Point.Point
        , velocity :: Point.Point
        , rotation :: Double
        , rotationVelocity :: Double
        , isInitialized :: Bool
        , arrivingDirection :: ArrivingDirection
        , hasArrived :: Bool
        }

    new :: Asteroid
    new = Asteroid
        { position = Point { x = 0, y = 0 }
        , velocity = Point { x = 0, y = 0 }
        , rotation = 0
        , rotationVelocity = 0
        , isInitialized = False
        , arrivingDirection = fromLeft
        , hasArrived = False
        }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Asteroid", "Resources/Asteroid.png")

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
        { minPositionX :: Double
        , maxPositionX :: Double
        , minPositionY :: Double
        , maxPositionY :: Double 

        , minVelocityX :: Double
        , maxVelocityX :: Double
        , minVelocityY :: Double
        , maxVelocityY :: Double

        , isInBounds :: Point.Point -> Bool
        , orientation :: Orientation
        }

    data Orientation = Horizontal | Vertical deriving Eq

    fromLeft :: ArrivingDirection
    fromLeft = ArrivingDirection
        { minPositionX = -arrivalMargin
        , maxPositionX = -arrivalMargin
        , minPositionY = arrivalMargin
        , maxPositionY = Constants.nativeHeight - arrivalMargin

        , minVelocityX = minVelocity
        , maxVelocityX = maxVelocity
        , minVelocityY = -maxVelocity * arrivalVelocityMultiplier
        , maxVelocityY = maxVelocity * arrivalVelocityMultiplier

        , isInBounds = isInBoundsFromLeft
        , orientation = Horizontal
        }

    isInBoundsFromLeft :: Point.Point -> Bool
    isInBoundsFromLeft Point{x} = x > arrivalMargin 

    fromRight :: ArrivingDirection
    fromRight = ArrivingDirection
        { minPositionX = Constants.nativeWidth + arrivalMargin
        , maxPositionX = Constants.nativeWidth + arrivalMargin
        , minPositionY = arrivalMargin
        , maxPositionY = Constants.nativeHeight - arrivalMargin

        , minVelocityX = -maxVelocity
        , maxVelocityX = -minVelocity
        , minVelocityY = -maxVelocity * arrivalVelocityMultiplier
        , maxVelocityY = maxVelocity * arrivalVelocityMultiplier

        , isInBounds = isInBoundsFromRight
        , orientation = Horizontal
        }

    isInBoundsFromRight :: Point.Point -> Bool
    isInBoundsFromRight Point{x} = x < Constants.nativeWidth - arrivalMargin

    fromTop :: ArrivingDirection
    fromTop = ArrivingDirection
        { minPositionX = arrivalMargin
        , maxPositionX = Constants.nativeWidth - arrivalMargin
        , minPositionY = -arrivalMargin
        , maxPositionY = -arrivalMargin

        , minVelocityX = -maxVelocity * arrivalVelocityMultiplier
        , maxVelocityX = maxVelocity * arrivalVelocityMultiplier
        , minVelocityY = minVelocity
        , maxVelocityY = maxVelocity

        , isInBounds = isInBoundsFromTop
        , orientation = Vertical
        }

    isInBoundsFromTop :: Point.Point -> Bool
    isInBoundsFromTop Point{y} = y > arrivalMargin

    fromBottom :: ArrivingDirection
    fromBottom = ArrivingDirection 
        { minPositionX = arrivalMargin
        , maxPositionX = Constants.nativeWidth - arrivalMargin
        , minPositionY = Constants.nativeHeight + arrivalMargin
        , maxPositionY = Constants.nativeHeight + arrivalMargin

        , minVelocityX = -maxVelocity * arrivalVelocityMultiplier
        , maxVelocityX = maxVelocity * arrivalVelocityMultiplier
        , minVelocityY = -maxVelocity
        , maxVelocityY = -minVelocity

        , isInBounds = isInBoundsFromBottom
        , orientation = Vertical
        }

    isInBoundsFromBottom :: Point.Point -> Bool
    isInBoundsFromBottom Point{y} = y < Constants.nativeHeight - arrivalMargin

    arrivingDirections :: [ArrivingDirection]
    arrivingDirections = [fromLeft, fromRight, fromTop, fromBottom]

    update' :: Asteroid -> Input -> Asteroid
    update' asteroid@Asteroid{position, velocity, rotation, rotationVelocity, isInitialized, hasArrived, arrivingDirection} Input{deltaTime, randomGenerator} =
        let
            (position', velocity', rotationVelocity', arrivingDirection') = if not isInitialized
                then
                    let
                        (arrivingDirectionIndex, randomGenerator1) = Random.randomR (0, length arrivingDirections - 1) randomGenerator
                        arrivingDirection = arrivingDirections !! arrivingDirectionIndex
                        (positionX, randomGenerator2) = Random.randomR (minPositionX arrivingDirection, maxPositionX arrivingDirection) randomGenerator1
                        (positionY, randomGenerator3) = Random.randomR (minPositionY arrivingDirection, maxPositionY arrivingDirection) randomGenerator2
                        (velocityX, randomGenerator4) = Random.randomR (minVelocityX arrivingDirection, maxVelocityX arrivingDirection) randomGenerator3
                        (velocityY, randomGenerator5) = Random.randomR (minVelocityY arrivingDirection, maxVelocityY arrivingDirection) randomGenerator4
                        (rotationVelocity, _) = Random.randomR (-maxRotationVelocity, maxRotationVelocity) randomGenerator5
                    in
                        (Point { x = positionX, y = positionY }, Point { x = velocityX, y = velocityY }, rotationVelocity, arrivingDirection)
                else
                    (position, velocity, rotationVelocity, arrivingDirection)

            hasArrived' = hasArrived || (isInBounds arrivingDirection' $ position')

            nextX = (Point.x position') + (Point.x velocity * deltaTime)
            nextY = (Point.y position') + (Point.y velocity * deltaTime)
            position'' = if not hasArrived'
                then Point { x = nextX, y = nextY }
                else Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }

            rotation' = rotation + (rotationVelocity * deltaTime)
        in
            asteroid
                { position = position''
                , velocity = velocity'
                , rotation = rotation'
                , rotationVelocity = rotationVelocity'
                , isInitialized = True
                , arrivingDirection = arrivingDirection' 
                , hasArrived = hasArrived'
                }

    drawAtPosition :: Asteroid -> Resources -> Point.Point -> Canvas.Picture ()
    drawAtPosition Asteroid{rotation} Resources{images} Point{x, y} = 
        let
            (bitmap, (width, height)) = images ! (fst imageDef)
            drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
            rotatedSprite = Canvas.rotate rotation drawnSprite
            translatedSprite = Canvas.translate (x, y) rotatedSprite
        in
            translatedSprite

    instance EntityClass Asteroid where

        load _ = [imageDef]

        update asteroid input = Entity $ update' asteroid input

        render asteroid@Asteroid{position, isInitialized, hasArrived, arrivingDirection} resources@Resources{images} = do
            when isInitialized (drawAtPosition asteroid resources position)

            let isVertical = orientation arrivingDirection == Vertical
            let (_, (width, height)) = images ! (fst imageDef)
            let x = Point.x position
            let y = Point.y position
            when
                (isInitialized && (hasArrived || isVertical) && x < width / 2)
                (drawAtPosition asteroid resources Point { x = Constants.nativeWidth + x, y = y })
            when 
                (isInitialized && (hasArrived || isVertical) && x > Constants.nativeWidth - (width / 2))
                (drawAtPosition asteroid resources Point { x = x - Constants.nativeWidth, y = y })
            when 
                (isInitialized && (hasArrived || not isVertical) && y < height / 2)
                (drawAtPosition asteroid resources Point { x = x, y = Constants.nativeHeight + y })
            when 
                (isInitialized && (hasArrived || not isVertical) && y > Constants.nativeHeight - (height / 2)) 
                (drawAtPosition asteroid resources Point { x = x, y = y - Constants.nativeHeight })