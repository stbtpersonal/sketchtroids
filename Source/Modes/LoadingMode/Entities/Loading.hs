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

    data Loading = Loading { elapsedTime :: Double, rotation :: Double }

    new :: Loading
    new = Loading { elapsedTime = 0, rotation = 0 }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Loading", "Resources/Loading.png")

    totalTime :: Double
    totalTime = 2000

    restingTime :: Double
    restingTime = 800

    instance EntityClass Loading where

        load _ = [imageDef]

        update loading@Loading{elapsedTime} input@Input{deltaTime} = 
            let
                moduloElapsedTime = (elapsedTime  + deltaTime) `mod'` totalTime
                phase = if moduloElapsedTime <= restingTime then 0 else (moduloElapsedTime - restingTime) / (totalTime - restingTime)
                updatedRotation = -(28.43811 * phase) + (188.4375 * phase ^ 2) - (274.2243 * phase ^ 3) + (120.5082 * phase ^ 4)
            in
                Entity $ loading { elapsedTime = moduloElapsedTime, rotation = updatedRotation }

        render loading@Loading{rotation} resources@Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                rotatedSprite = Canvas.rotate rotation drawnSprite
                translatedSprite = Canvas.translate (Constants.nativeWidth / 2, Constants.nativeHeight / 2) rotatedSprite
            in
                translatedSprite