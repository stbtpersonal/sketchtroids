{-# LANGUAGE NamedFieldPuns #-}

module LoadingMode(
    LoadingMode(LoadingMode),
    LoadingMode.new,
    LoadingMode.imageDefs) where

    import Entity
    import Resources
    import GameMode
    import Data.Map as Map
    import Input
    import Background
    import Fps
    import Spinner as Spinner
    import Haste.Graphics.Canvas as Canvas

    data LoadingMode = LoadingMode { children :: [Entity], spinner :: Spinner, finishElapsedTime :: Double, finishAlpha :: Double }

    new :: LoadingMode
    new = LoadingMode { children = [Entity Background.new, Entity Fps.new], spinner = Spinner.new, finishElapsedTime = 0, finishAlpha = 1 }

    imageDefs :: [Resources.ResourceDef]
    imageDefs =
        let
            loadingMode@LoadingMode{children, spinner} = LoadingMode.new
        in
            Entity.loadAll (Entity spinner : children)

    finishTotalTime :: Double
    finishTotalTime = 1000

    instance EntityClass LoadingMode where

        load loadingMode@LoadingMode{children} = GameMode.imageDefs

        update loadingMode@LoadingMode{children, spinner, finishElapsedTime} input@Input{resources, deltaTime} =
            let
                images = Resources.images resources
                loadedImageKeys = Map.keys images
                requiredImageKeys = Prelude.map fst GameMode.imageDefs
                areAllImagesLoaded = and $ Prelude.map (\key -> elem key loadedImageKeys) requiredImageKeys

                isSpinnerStopped = Spinner.isStopped spinner

                updatedFinishElapsedTime = if areAllImagesLoaded && isSpinnerStopped then finishElapsedTime + deltaTime else 0
                updatedFinishAlpha = 1 - (updatedFinishElapsedTime / finishTotalTime)
                isFinishedAnimating = updatedFinishElapsedTime > finishTotalTime

                updatedChildren = Entity.updateAll children input
                potentiallyStoppedSpinner = if areAllImagesLoaded then Spinner.stop spinner else spinner
                updatedSpinner = Spinner.update' potentiallyStoppedSpinner input
            in
                if isFinishedAnimating
                    then Entity GameMode.new
                    else Entity $ loadingMode { children = updatedChildren, spinner = updatedSpinner, finishElapsedTime = updatedFinishElapsedTime, finishAlpha = updatedFinishAlpha }

        render LoadingMode{children, spinner, finishAlpha} input = 
            let
                renderedChildren = Entity.renderAll children input
                renderedSpinner = Entity.render (Entity spinner) input
                fadedSpinner = Canvas.opacity finishAlpha renderedSpinner
            in
                renderedChildren >> fadedSpinner