{-# LANGUAGE NamedFieldPuns #-}

module Modes.GameMode.Entities.Bullet(
    Bullet(Bullet, position, velocity),
    new
    ) where

    import Point
    import Entity
    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas

    data Bullet = Bullet { position :: Point.Point, velocity :: Point.Point }

    new :: Bullet
    new = Bullet { position = Point { x = 0, y = 0 }, velocity = Point { x = 0, y = 0 } }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Bullet", "Resources/Bullet.png")

    instance EntityClass Bullet where

        load _ = [imageDef]

        update bullet@Bullet{position, velocity} input@Input{deltaTime} =
            let
                updatedPosition = Point { x = (Point.x position) + (Point.x velocity * deltaTime), y = (Point.y position) + (Point.y velocity * deltaTime) }
            in
                Entity $ bullet { position = updatedPosition }

        render bullet@Bullet{position, velocity} resources@Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotation = Point.angle velocity
                rotatedSprite = Canvas.rotate rotation drawnSprite
                translatedSprite = Canvas.translate (Point.x position, Point.y position) rotatedSprite
            in
                translatedSprite