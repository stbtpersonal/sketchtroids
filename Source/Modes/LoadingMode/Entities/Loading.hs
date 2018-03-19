{-# LANGUAGE NamedFieldPuns #-}

module Modes.LoadingMode.Entities.Loading(Loading(Loading), new) where

    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Constants
    import Haste.DOM as DOM
    import Haste
    import Data.Fixed

    data Loading = Loading { elapsedTime :: Double, rotation :: Double, scale :: Double }

    new :: Loading
    new = Loading { elapsedTime = 0, rotation = 0, Modes.LoadingMode.Entities.Loading.scale = 1 }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Loading", "Resources/Loading.png")

    rotationStartTime :: Double
    rotationStartTime = 800

    scaleStartTime :: Double
    scaleStartTime = 1900

    totalTime :: Double
    totalTime = 2000

    instance EntityClass Loading where

        load _ = [imageDef]

        update loading@Loading{elapsedTime} input@Input{deltaTime} = 
            let
                moduloElapsedTime = (elapsedTime  + deltaTime) `mod'` totalTime
                rotationPhase = if moduloElapsedTime <= rotationStartTime then 0 else (moduloElapsedTime - rotationStartTime) / (totalTime - rotationStartTime)
                rotation = -(28.43811 * rotationPhase) + (188.4375 * rotationPhase ^ 2) - (274.2243 * rotationPhase ^ 3) + (120.5082 * rotationPhase ^ 4)
                scalePhase = if moduloElapsedTime <= scaleStartTime then 1 else (moduloElapsedTime - scaleStartTime) / (totalTime - scaleStartTime)
                scale = 1 + (0.1 * scalePhase) - (0.1 * scalePhase ^ 2)
            in
                Entity $ loading { elapsedTime = moduloElapsedTime, rotation = rotation, Modes.LoadingMode.Entities.Loading.scale = scale }

        render loading@Loading{rotation, Modes.LoadingMode.Entities.Loading.scale} resources@Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                scaledSprite = Canvas.scale (scale, scale) rotatedSprite
                translatedSprite = Canvas.translate (Constants.nativeWidth / 2, Constants.nativeHeight / 2) scaledSprite
            in
                translatedSprite