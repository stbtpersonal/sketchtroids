module Main where

    import Control.Monad
    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Game
    import Background
    import Fps
    import SpinningRectangle
    import Point
    import Entity
    import Renderer

    nativeWidth :: Double
    nativeWidth = 800

    nativeHeight :: Double
    nativeHeight = 600

    main :: IO ()
    main = do
        canvas <- Renderer.initialize nativeWidth nativeHeight

        let background = Entity $ Background.new nativeWidth nativeHeight
        let spinningRectangle1 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 50, Point.y = 50 }, SpinningRectangle.speed = 0.001 }
        let spinningRectangle2 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 150, Point.y = 50 }, SpinningRectangle.speed = 0.002 }
        let spinningRectangle3 = Entity $ SpinningRectangle.new { SpinningRectangle.position = Point.Point { Point.x = 50, Point.y = 150 }, SpinningRectangle.speed = 0.003 }
        let fps = Entity $ Fps.new

        let game = (Game.new canvas) { Game.entities = [background, spinningRectangle1, spinningRectangle2, spinningRectangle3, fps] }

        AnimationFrame.requestAnimationFrame $ Main.mainLoop game
        return ()

    mainLoop :: Game -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop game timestamp = do
        let previousTimestamp = Game.timestamp game
        let deltaTime = timestamp - previousTimestamp

        let entities = Game.entities game
        let updatedEntities = map (\entity -> Entity.update entity deltaTime) entities
        Renderer.render (Game.canvas game) $ mapM Entity.render updatedEntities

        let updatedGame = game { Game.timestamp = timestamp, Game.entities = updatedEntities }

        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedGame
        return ()