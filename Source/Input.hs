{-# LANGUAGE NamedFieldPuns #-}

module Input
    ( Input(Input, deltaTime, resources, keyboard, randomGenerator, isDebugEnabled)
    , Input.randomizeNext
    , Input.randomize
    ) where

    import Resources
    import Keyboard
    import System.Random as Random
    import Utils

    data Input = Input
        { deltaTime :: Double
        , resources :: Resources
        , keyboard :: Keyboard
        , randomGenerator :: Random.StdGen
        , isDebugEnabled :: Bool
        }

    randomizeNext :: Input -> Input
    randomizeNext input@Input{randomGenerator} = input { randomGenerator = head $ Utils.splits randomGenerator }

    randomize :: Input -> [Input]
    randomize input = input : randomize (randomizeNext input)