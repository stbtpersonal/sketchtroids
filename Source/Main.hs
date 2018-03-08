module Main where

    import Control.Monad
    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Game
    import Entity
    import Renderer
    import Input

    nativeWidth :: Double
    nativeWidth = 800

    nativeHeight :: Double
    nativeHeight = 600

    main :: IO ()
    main = do
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

        let updatedGame = game { Game.timestamp = timestamp, Game.entities = updatedEntities }

        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedGame
        return ()