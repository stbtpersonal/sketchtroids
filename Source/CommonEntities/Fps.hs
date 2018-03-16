{-# LANGUAGE RecordWildCards #-}

module CommonEntities.Fps where
    
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Input

    data Fps = Fps { timeCount :: Double, frameCount :: Integer, fpsToDisplay :: String }

    new :: Fps
    new = Fps { timeCount = 0, frameCount = 0, fpsToDisplay = "0" }

    instance EntityClass Fps where

        update fps@Fps{..} input@Input{..} =
            let
                addedTimeCount = timeCount + deltaTime
                addedFrameCount = frameCount + 1
                (updatedTimeCount, updatedFrameCount, updatedFpsToDisplay)
                    | addedTimeCount < 1000 = (addedTimeCount, addedFrameCount, fpsToDisplay)
                    | otherwise             = (addedTimeCount - 1000, 1, show addedFrameCount)
            in
                Entity $ fps { timeCount = updatedTimeCount, frameCount = updatedFrameCount, fpsToDisplay = updatedFpsToDisplay }

        render fps@Fps{..} _ =
            let
                text = Canvas.text (5, 22.5) $ "FPS: " ++ fpsToDisplay
                fonted = Canvas.font "20px sans-serif" text
            in
                fonted