{-# LANGUAGE RecordWildCards #-}

module Main
    ( Main.main
    ) where

    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Entity
    import Renderer
    import Input
    import Resources
    import Haste.Graphics.Canvas as Canvas
    import FrameData
    import PreloadingMode
    import Keyboard
    import Data.IORef as IORef
    import System.Random as Random
    import Debug

    main :: IO ()
    main = do
        resourcesRef <- Resources.initialize
        keyboardRef <- Keyboard.initialize
        canvas <- Renderer.initialize
        let initialFrameData = FrameData { canvas = canvas, timestamp = 0, resourcesRef = resourcesRef, keyboardRef = keyboardRef, mode = Entity PreloadingMode.new }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop initialFrameData
        return ()

    mainLoop :: FrameData -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop frameData@FrameData{..} nextTimestamp = do
        let deltaTime = nextTimestamp - timestamp

        Resources.loadResources resourcesRef $ Entity.load mode

        resources <- IORef.readIORef resourcesRef
        keyboard <- IORef.readIORef keyboardRef
        randomGenerator <- Random.newStdGen
        isDebugEnabled <- Debug.isDebugEnabled
        let input = Input { deltaTime = deltaTime, resources = resources, keyboard = keyboard, randomGenerator = randomGenerator, isDebugEnabled = isDebugEnabled }
        let updatedMode = Entity.update mode input

        scale <- Renderer.resize canvas
        let scaledPicture = Canvas.scale (scale, scale) (Entity.render updatedMode input)
        Renderer.render canvas scaledPicture

        let updatedFrameData = frameData { timestamp = nextTimestamp, mode = updatedMode }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedFrameData
        return ()