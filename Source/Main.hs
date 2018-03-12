module Main where

    import Control.Monad
    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Game
    import Entity
    import Renderer
    import Input
    import Haste
    import Resources
    import Haste.Graphics.Canvas as Canvas

    nativeWidth :: Double
    nativeWidth = 800

    nativeHeight :: Double
    nativeHeight = 600

    main :: IO ()
    main = do
        Resources.loadResources
        canvas <- Renderer.initialize nativeWidth nativeHeight
        let game = Game.new canvas nativeWidth nativeHeight
        AnimationFrame.requestAnimationFrame $ Main.mainLoop game
        return ()

    mainLoop :: Game -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop game timestamp = do
        let previousTimestamp = Game.timestamp game
        let deltaTime = timestamp - previousTimestamp

        input <- Input.poll deltaTime
        let updatedEntities = Entity.updateAll (Game.entities game) input
        Renderer.render (Game.canvas game) (Entity.renderAll updatedEntities)

        resourcesAreLoaded <- Resources.areResourcesLoaded
        Haste.writeLog $ Haste.toJSString ("areImagesLoaded: " ++ (show resourcesAreLoaded))

        when resourcesAreLoaded $ do
            resources <- Resources.getResources
            let images = Resources.images resources
            let firstResource = images !! 0
            let firstBitmap = snd firstResource
            Canvas.renderOnTop (Game.canvas game) (Canvas.draw firstBitmap (50, 50))
            let secondResource = images !! 1
            let secondBitmap = snd secondResource
            Canvas.renderOnTop (Game.canvas game) (Canvas.draw secondBitmap (150, 50))

        let updatedGame = game { Game.timestamp = timestamp, Game.entities = updatedEntities }

        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedGame
        return ()