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

    main :: IO ()
    main = do
        canvas <- Renderer.initialize
        let initialFrameData = FrameData { canvas = canvas, timestamp = 0, resources = Resources.empty, mode = Entity LoadingMode.new }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop initialFrameData
        return ()

    mainLoop :: FrameData -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop frameData@FrameData{..} nextTimestamp = do
        let deltaTime = nextTimestamp - timestamp

        let imageKeysToPaths = Entity.load mode
        Resources.loadResources imageKeysToPaths 
        updatedResources <- Resources.appendResources imageKeysToPaths resources

        input <- Input.poll deltaTime updatedResources
        let updatedMode = Entity.update mode input

        scale <- Renderer.resize canvas
        let scaledPicture = Canvas.scale (scale, scale) (Entity.render updatedMode)
        Renderer.render canvas scaledPicture

        let updatedFrameData = frameData { timestamp = nextTimestamp, FrameData.resources = updatedResources, mode = updatedMode }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedFrameData
        return ()