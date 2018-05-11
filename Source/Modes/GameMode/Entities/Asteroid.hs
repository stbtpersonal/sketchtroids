{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Asteroid(
    Asteroid(),
    new
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

    data Asteroid = Asteroid { position :: Point.Point
                             , velocity :: Point.Point
                             , rotation :: Double
                             , rotationVelocity :: Double
                             , wasInitialized :: Bool
                             }

    new :: Asteroid
    new = Asteroid { position = Point { x = 600, y = 400 }
                   , velocity = Point { x = 0, y = 0 }
                   , rotation = 0
                   , rotationVelocity = 0
                   , wasInitialized = False
                   }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Asteroid", "Resources/Asteroid.png")

    maxVelocity :: Double
    maxVelocity = 0.1

    maxRotationVelocity :: Double
    maxRotationVelocity = 0.001

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

        update asteroid@Asteroid{position, velocity, rotation, rotationVelocity, wasInitialized} Input{deltaTime, randomGenerator} =
            let
                (velocity', rotationVelocity', _) = if not wasInitialized
                    then
                        let
                            (velocityX, randomGenerator') = Random.randomR (-maxVelocity, maxVelocity) randomGenerator
                            (velocityY, randomGenerator'') = Random.randomR (-maxVelocity, maxVelocity) randomGenerator'
                            (rotationVelocity, randomGenerator''') = Random.randomR (-maxRotationVelocity, maxRotationVelocity) randomGenerator''
                        in
                            (Point { x = velocityX, y = velocityY }, rotationVelocity, randomGenerator''')
                    else
                        (velocity, rotationVelocity, randomGenerator)

                nextX = (Point.x position) + (Point.x velocity * deltaTime)
                nextY = (Point.y position) + (Point.y velocity * deltaTime)
                position' = Point { x = Utils.wrap 0 Constants.nativeWidth nextX, y = Utils.wrap 0 Constants.nativeHeight nextY }
                rotation' = rotation + (rotationVelocity * deltaTime)
            in
                Entity $ asteroid { position = position', velocity = velocity', rotation = rotation', rotationVelocity = rotationVelocity', wasInitialized = True }

        render asteroid@Asteroid{position} resources@Resources{images} = do
            drawAtPosition asteroid resources position

            let (_, (width, height)) = images ! (fst imageDef)
            let x = Point.x position
            let y = Point.y position
            when (x < width / 2) (drawAtPosition asteroid resources Point { x = Constants.nativeWidth + x, y = y })
            when (x > Constants.nativeWidth - (width / 2)) (drawAtPosition asteroid resources Point { x = x - Constants.nativeWidth, y = y })
            when (y < height / 2) (drawAtPosition asteroid resources Point { x = x, y = Constants.nativeHeight + y })
            when (y > Constants.nativeHeight - (height / 2)) (drawAtPosition asteroid resources Point { x = x, y = y - Constants.nativeHeight })