{-# LANGUAGE NamedFieldPuns #-}

module Modes.LoadingMode.Entities.Spinner(Spinner(Spinner), new) where

    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Constants
    import Haste.DOM as DOM
    import Haste
    import Data.Fixed

    data Spinner = Spinner { elapsedTime :: Double, rotation :: Double, scale :: Double }

    new :: Spinner
    new = Spinner { elapsedTime = 0, rotation = 0, Modes.LoadingMode.Entities.Spinner.scale = 1 }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Spinner", "Resources/Spinner.png")

    rotationStartTime :: Double
    rotationStartTime = 800

    scaleStartTime :: Double
    scaleStartTime = 1900

    totalTime :: Double
    totalTime = 2000

    instance EntityClass Spinner where

        load _ = [imageDef]

        update spinner@Spinner{elapsedTime} input@Input{deltaTime} = 
            let
                moduloElapsedTime = (elapsedTime  + deltaTime) `mod'` totalTime
                rotationPhase = if moduloElapsedTime <= rotationStartTime then 0 else (moduloElapsedTime - rotationStartTime) / (totalTime - rotationStartTime)
                rotation = -(28.43811 * rotationPhase) + (188.4375 * rotationPhase ^ 2) - (274.2243 * rotationPhase ^ 3) + (120.5082 * rotationPhase ^ 4)
                scalePhase = if moduloElapsedTime <= scaleStartTime then 1 else (moduloElapsedTime - scaleStartTime) / (totalTime - scaleStartTime)
                scale = 1 + (0.1 * scalePhase) - (0.1 * scalePhase ^ 2)
            in
                Entity $ spinner { elapsedTime = moduloElapsedTime, rotation = rotation, Modes.LoadingMode.Entities.Spinner.scale = scale }

        render spinner@Spinner{rotation, Modes.LoadingMode.Entities.Spinner.scale} resources@Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                scaledSprite = Canvas.scale (scale, scale) rotatedSprite
                translatedSprite = Canvas.translate (Constants.nativeWidth / 2, Constants.nativeHeight / 2) scaledSprite
            in
                translatedSprite