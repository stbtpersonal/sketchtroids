{-# LANGUAGE RecordWildCards #-}

module Main where

    import Control.Monad
    import Haste.Graphics.AnimationFrame as AnimationFrame
    import GameMode
    import Entity
    import Renderer
    import Input
    import Haste
    import Resources
    import Haste.Graphics.Canvas as Canvas
    import Data.Map as Map
    import ResourceKeys

    data FrameData = FrameData { canvas :: Canvas, timestamp :: Double, mode :: Entity }

    main :: IO ()
    main = do
        -- Remember: loadResources should be called only once, at the beginning of the loading state
        Resources.loadResources

        canvas <- Renderer.initialize
        let initialFrameData = FrameData { canvas = canvas, timestamp = 0, mode = Entity GameMode.new }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop initialFrameData
        return ()

    mainLoop :: FrameData -> AnimationFrame.HRTimeStamp -> IO ()
    mainLoop frameData@FrameData{..} nextTimestamp = do
        let deltaTime = nextTimestamp - timestamp

        input <- Input.poll deltaTime
        let updatedMode = Entity.update mode input
        Renderer.render canvas (Entity.render updatedMode)

        -- Remember: getResources should be called only once, at the end of the loading state
        areResourcesLoaded <- Resources.areResourcesLoaded
        when areResourcesLoaded $ do
            resources <- Resources.getResources
            let images = Resources.images resources
            let spaceshipBitmap = images ! ResourceKeys.Spaceship
            Canvas.renderOnTop canvas (Canvas.draw spaceshipBitmap (50, 50))
            let bulletBitmap = images ! ResourceKeys.Bullet
            Canvas.renderOnTop canvas (Canvas.draw bulletBitmap (150, 50))

        let updatedFrameData = frameData { timestamp = nextTimestamp, mode = updatedMode }
        AnimationFrame.requestAnimationFrame $ Main.mainLoop updatedFrameData
        return ()