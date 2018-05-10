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

    data Asteroid = Asteroid { position :: Point.Point
                             , velocity :: Point.Point
                             , rotation :: Double
                             , rotationVelocity :: Double
                             }

    new :: Asteroid
    new = Asteroid { position = Point { x = 600, y = 400 }
                   , velocity = Point { x = 0, y = 0 }
                   , rotation = 0
                   , rotationVelocity = 0.0001
                   }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Asteroid", "Resources/Asteroid.png")

    instance EntityClass Asteroid where

        load _ = [imageDef]

        update asteroid@Asteroid{position, velocity, rotation, rotationVelocity} Input{deltaTime} =
            let
                updatedPosition = Point { x = (Point.x position) + (Point.x velocity * deltaTime), y = (Point.y position) + (Point.y velocity * deltaTime) }
                updatedRotation = rotation + (rotationVelocity * deltaTime)
            in
                Entity $ asteroid { position = updatedPosition, rotation = updatedRotation }

        render Asteroid{position, rotation} Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                translatedSprite = Canvas.translate (Point.x position, Point.y position) rotatedSprite
            in
                translatedSprite