{-# LANGUAGE NamedFieldPuns #-}

module Explosion
    ( Explosion()
    , Explosion.new
    , Explosion.isDone
    ) where

    import Entity
    import Sprite
    import Resources
    import Haste.Graphics.Canvas as Canvas hiding (Point)
    import Utils
    import Input
    import Point
    import System.Random as Random

    data Explosion = Explosion
        { _position :: Point
        , _rotation :: Double
        , _imageDef :: Resources.ResourceDef
        , _duration :: Double
        , _timeElapsed :: Double
        , _alpha :: Double
        }

    new :: Point -> Resources.ResourceDef -> Double -> Input -> Explosion
    new position imageDef duration Input{randomGenerator} = Explosion
        { _position = position
        , _rotation = fst $ Random.randomR (0, pi * 2) randomGenerator
        , _imageDef = imageDef
        , _duration = duration
        , _timeElapsed = 0
        , _alpha = 1
        }

    isDone :: Explosion -> Bool
    isDone Explosion{_alpha} = _alpha == 0

    instance EntityClass Explosion where
        load explosion = Sprite.imageDefs explosion
        update explosion input = Entity $ Sprite.update explosion input
        render explosion@Explosion{_alpha} input = Canvas.opacity _alpha $ Sprite.render explosion input

    instance Sprite Explosion where
        imageDef Explosion{_imageDef} = _imageDef
        position Explosion{_position} = _position
        setPosition explosion position = explosion{_position = position}
        rotation Explosion{_rotation} = _rotation
        update explosion@Explosion{_timeElapsed, _duration} Input{deltaTime} =
            let
                timeElapsed' = Utils.clamp 0 _duration (_timeElapsed + deltaTime)
                alpha' = 1 - (_timeElapsed / _duration)
            in
                explosion
                { _timeElapsed = timeElapsed'
                , _alpha = alpha'
                }