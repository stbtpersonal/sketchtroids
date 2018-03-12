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
    import Data.Map as Map
    import ResourceKeys

    nativeWidth :: Double
    nativeWidth = 800

    nativeHeight :: Double
    nativeHeight = 600

    main :: IO ()
    main = do
        -- Remember: loadResources should be called only once, at the beginning of the loading state
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

        -- Remember: getResources should be called only once, at the end of the loading state
        areResourcesLoaded <- Resources.areResourcesLoaded
        when areResourcesLoaded $ do
            resources <- Resources.getResources
            let images = Resources.images resources
            let spaceshipBitmap = images ! ResourceKeys.Spaceship
            Canvas.renderOnTop (Game.canvas game) (Canvas.draw spaceshipBitmap (50, 50))
            let bulletBitmap = images ! ResourceKeys.Bullet
            Canvas.renderOnTop (Game.canvas game) (Canvas.draw bulletBitmap (150, 50))

        let updatedGame = game { Game.timestamp = timestamp, Game.entities = updatedEntities }

        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedGame
        return ()