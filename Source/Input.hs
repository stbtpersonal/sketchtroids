module Input where

    import Foreign.Keyboard as Keyboard

    data Input = Input { deltaTime :: Double, left :: Bool, right :: Bool, up :: Bool, down :: Bool }

    leftKeys :: [String]
    leftKeys = ["KeyA", "ArrowLeft", "Numpad4", "KeyJ"]

    rightKeys :: [String]
    rightKeys = ["KeyD", "ArrowRight", "Numpad6", "KeyL"]

    upKeys :: [String]
    upKeys = ["KeyW", "ArrowUp", "Numpad8", "KeyI"]

    downKeys :: [String]
    downKeys = ["KeyS", "ArrowDown", "Numpad5", "Numpad2", "KeyK"]

    poll :: Double -> IO Input
    poll deltaTime = do
        left <- pollKeys leftKeys
        right <- pollKeys rightKeys
        up <- pollKeys upKeys
        down <- pollKeys downKeys
        return Input { deltaTime = deltaTime, left = left, right = right, up = up, down = down }

    pollKeys :: [String] -> IO Bool
    pollKeys keys = do
        keyStates <- mapM Keyboard.isKeyDown keys
        return $ or keyStates