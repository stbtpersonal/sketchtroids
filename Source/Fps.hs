{-# LANGUAGE NamedFieldPuns #-}

module Fps where
    
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Input

    data Fps = Fps
        { timeCount :: Double
        , frameCount :: Integer
        , fpsToDisplay :: String
        }

    new :: Fps
    new = Fps 
        { timeCount = 0
        , frameCount = 0
        , fpsToDisplay = "0"
        }

    update' :: Fps -> Input -> Fps
    update' fps@Fps{timeCount, frameCount, fpsToDisplay} input@Input{deltaTime} =
        let
            timeCount' = timeCount + deltaTime
            frameCount' = frameCount + 1
            (timeCount'', frameCount'', fpsToDisplay')
                | timeCount' < 1000 = (timeCount', frameCount', fpsToDisplay)
                | otherwise         = (timeCount' - 1000, 1, show frameCount')
        in
            fps
                { timeCount = timeCount''
                , frameCount = frameCount''
                , fpsToDisplay = fpsToDisplay' 
                }

    instance EntityClass Fps where

        update fps input = Entity $ update' fps input

        render fps@Fps{fpsToDisplay} _ =
            let
                text = Canvas.text (5, 22.5) $ "FPS: " ++ fpsToDisplay
                fonted = Canvas.font "20px sans-serif" text
            in
                fonted