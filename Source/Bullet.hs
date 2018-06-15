{-# LANGUAGE NamedFieldPuns #-}

module Bullet
    ( Bullet(Bullet)
    , new
    , update'
    , isOutOfBounds
    , imageDef'
    ) where

    import Point
    import Entity
    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas
    import Sprite
    import Constants
    import Collidable

    data Bullet = Bullet
        { _position :: Point.Point
        , _velocity :: Point.Point
        } deriving (Eq)

    new :: Point.Point -> Point.Point -> Bullet
    new position velocity = Bullet 
        { _position = position
        , _velocity = velocity
        }
    
    imageDef' :: Resources.ResourceDef
    imageDef' = (ResourceKey "Bullet", "Resources/Bullet.png")

    update' :: Bullet -> Input -> Bullet
    update' bullet@Bullet{_position, _velocity} input@Input{deltaTime} = 
        let
            position' = Point { x = (Point.x _position) + (Point.x _velocity * deltaTime), y = (Point.y _position) + (Point.y _velocity * deltaTime) }
        in
            bullet { _position = position' }

    isOutOfBounds :: Bullet -> Resources -> Bool
    isOutOfBounds bullet@Bullet{_position} resources = 
        let
            (width, height) = Sprite.dimensions bullet resources
            Point{x, y} = _position
        in
            x < -width || x > Constants.nativeWidth + width || y < -height || y > Constants.nativeHeight + height

    instance EntityClass Bullet where
        load bullet = imageDefs bullet
        update bullet input = Entity $ update' bullet input
        render bullet resources = Collidable.render bullet resources

    instance Sprite Bullet where
        imageDef _ = imageDef'
        position Bullet{_position} = _position
        rotation Bullet{_velocity} = Point.angle _velocity

    instance Collidable Bullet where