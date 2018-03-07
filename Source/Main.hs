module Main where

    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Game
    import Entity
    import Renderer
    import Haste
    import Foreign.Keyboard as Keyboard

    nativeWidth :: Double
    nativeWidth = 800

    nativeHeight :: Double
    nativeHeight = 600

    main :: IO ()
    main = do
        isADown <- Keyboard.isKeyDown "a"
        isBDown <- Keyboard.isKeyDown "b"
        isCDown <- Keyboard.isKeyDown "c"
        Haste.writeLog $ Haste.toJSString ("Is a down: " ++ show isADown)
        Haste.writeLog $ Haste.toJSString ("Is b down: " ++ show isBDown)
        Haste.writeLog $ Haste.toJSString ("Is c down: " ++ show isCDown)

        canvas <- Renderer.initialize nativeWidth nativeHeight
        let game = Game.new canvas nativeWidth nativeHeight
        AnimationFrame.requestAnimationFrame $ Main.mainLoop game
        return ()

    mainLoop :: Game -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop game timestamp = do
        let previousTimestamp = Game.timestamp game
        let deltaTime = timestamp - previousTimestamp

        let updatedEntities = Entity.updateAll (Game.entities game) deltaTime
        Renderer.render (Game.canvas game) (Entity.renderAll updatedEntities)

        let updatedGame = game { Game.timestamp = timestamp, Game.entities = updatedEntities }

        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedGame
        return ()