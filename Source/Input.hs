module Input where

    import Resources
    import Keyboard

    data Input = Input { deltaTime :: Double, resources :: Resources, keyboard :: Keyboard }