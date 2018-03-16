{-# LANGUAGE RecordWildCards #-}

module Main where

    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Entity
    import Renderer
    import Input
    import Resources
    import Haste.Graphics.Canvas as Canvas
    import FrameData
    import Modes.LoadingMode.LoadingMode as LoadingMode
    import Keyboard
    import Data.IORef as IORef

    main :: IO ()
    main = do
        resourcesRef <- Resources.initialize
        keyboardRef <- Keyboard.initialize
        canvas <- Renderer.initialize
        let initialFrameData = FrameData { canvas = canvas, timestamp = 0, resourcesRef = resourcesRef, keyboardRef = keyboardRef, mode = Entity LoadingMode.new }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop initialFrameData
        return ()

    mainLoop :: FrameData -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop frameData@FrameData{..} nextTimestamp = do
        let deltaTime = nextTimestamp - timestamp

        Resources.loadImages resourcesRef $ Entity.load mode

        resources <- IORef.readIORef resourcesRef
        keyboard <- IORef.readIORef keyboardRef
        let input = Input { deltaTime = deltaTime, resources = resources, keyboard = keyboard }
        let updatedMode = Entity.update mode input

        scale <- Renderer.resize canvas
        let scaledPicture = Canvas.scale (scale, scale) (Entity.render updatedMode)
        Renderer.render canvas scaledPicture

        let updatedFrameData = frameData { timestamp = nextTimestamp, mode = updatedMode }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedFrameData
        return ()