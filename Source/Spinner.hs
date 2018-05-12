{-# LANGUAGE NamedFieldPuns #-}

module Spinner(Spinner(Spinner, isStopped), new, stop, update') where

    import Resources
    import Input
    import Data.Map as Map
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Constants
    import Haste.DOM as DOM
    import Haste
    import Data.Fixed

    data Spinner = Spinner { elapsedTime :: Double, rotation :: Double, scale :: Double, isStopping :: Bool, isStopped :: Bool }

    new :: Spinner
    new = Spinner { elapsedTime = 0, rotation = 0, Spinner.scale = 1, isStopping = False, isStopped = False }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Spinner", "Resources/Spinner.png")

    rotationStartTime :: Double
    rotationStartTime = 200

    rotationEndTime :: Double
    rotationEndTime = 1400

    scaleStartTime :: Double
    scaleStartTime = 1300

    scaleEndTime :: Double
    scaleEndTime = 1400

    totalTime :: Double
    totalTime = 2000

    stop :: Spinner -> Spinner
    stop spinner = spinner { isStopping = True }

    update' :: Spinner -> Input -> Spinner
    update' spinner@Spinner{elapsedTime, isStopping} input@Input{deltaTime} = 
        let
            nextElapsedTime = elapsedTime + deltaTime
            isStopped = isStopping && nextElapsedTime > totalTime
            moduloElapsedTime = if isStopped then totalTime else nextElapsedTime `mod'` totalTime
            rotationPhase = if moduloElapsedTime <= rotationStartTime || moduloElapsedTime > rotationEndTime then 0 else (moduloElapsedTime - rotationStartTime) / (rotationEndTime - rotationStartTime)
            rotation = -(28.43811 * rotationPhase) + (188.4375 * rotationPhase ^ 2) - (274.2243 * rotationPhase ^ 3) + (120.5082 * rotationPhase ^ 4)
            scalePhase = if moduloElapsedTime <= scaleStartTime || moduloElapsedTime > scaleEndTime then 1 else (moduloElapsedTime - scaleStartTime) / (scaleEndTime - scaleStartTime)
            scale = 1 + (0.1 * scalePhase) - (0.1 * scalePhase ^ 2)
        in
            spinner { elapsedTime = moduloElapsedTime, rotation = rotation, Spinner.scale = scale, isStopped = isStopped }

    instance EntityClass Spinner where

        load _ = [imageDef]

        update spinner input = Entity $ update' spinner input

        render spinner@Spinner{rotation, Spinner.scale, isStopped} resources@Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                scaledSprite = Canvas.scale (scale, scale) rotatedSprite
                translatedSprite = Canvas.translate (Constants.nativeWidth / 2, Constants.nativeHeight / 2) scaledSprite
            in
                translatedSprite