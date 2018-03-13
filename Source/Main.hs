{-# LANGUAGE RecordWildCards #-}

module Main where

    import Haste.Graphics.AnimationFrame as AnimationFrame
    import Modes.GameMode.GameMode as GameMode
    import Entity
    import Renderer
    import Input
    import Resources
    import Haste.Graphics.Canvas as Canvas
    import FrameData

    main :: IO ()
    main = do
        canvas <- Renderer.initialize
        let initialFrameData = FrameData { canvas = canvas, timestamp = 0, resources = Resources.empty, mode = Entity GameMode.new }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop initialFrameData
        return ()

    mainLoop :: FrameData -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop frameData@FrameData{..} nextTimestamp = do
        let deltaTime = nextTimestamp - timestamp

        Resources.loadResources $ Entity.load mode
        updatedResources <- Resources.appendResources imageKeysToPaths resources

        input <- Input.poll deltaTime updatedResources
        let updatedMode = Entity.update mode input
        Renderer.render canvas (Entity.render updatedMode)

        let updatedFrameData = frameData { timestamp = nextTimestamp, FrameData.resources = updatedResources, mode = updatedMode }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedFrameData
        return ()