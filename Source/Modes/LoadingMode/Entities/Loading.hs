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

    data Loading = Loading { elapsedTime :: Double }

    new :: Loading
    new = Loading { elapsedTime = 0 }

    imageDef :: Resources.ResourceDef
    imageDef = (ResourceKey "Loading", "Resources/Loading.png")

    instance EntityClass Loading where

        load _ = [imageDef]

        update loading@Loading{elapsedTime} input@Input{deltaTime} = Entity $ loading { elapsedTime = elapsedTime + deltaTime }

        render _ resources@Resources{images} = 
            let
                (bitmap, (width, height)) = images ! (fst imageDef)
                drawnSprite = Canvas.draw bitmap (-(width / 2), -(height / 2))
                translatedSprite = Canvas.translate (Constants.nativeWidth / 2, Constants.nativeHeight / 2) drawnSprite
            in
                translatedSprite