module Keyboard (initialize, Keyboard(..)) where

    import Data.IORef as IORef
    import Haste.DOM as DOM
    import Haste.Events as Events
    import Haste

    data Keyboard = Keyboard { left :: Bool, right :: Bool, up :: Bool, down :: Bool }

    keyA = 65
    arrowLeft = 37
    numpad4 = 100
    keyJ = 74

    leftKeys :: [Int]
    leftKeys = [keyA, arrowLeft, numpad4, keyJ]

    keyD = 68
    arrowRight = 39
    numpad6 = 102
    keyL = 76

    rightKeys :: [Int]
    rightKeys = [keyD, arrowRight, numpad6, keyL]

    keyW = 87
    arrowUp = 38
    numpad8 = 104
    keyI = 73

    upKeys :: [Int]
    upKeys = [keyW, arrowUp, numpad8, keyI]

    keyS = 83
    arrowDown = 40
    numpad5 = 101
    numpad2 = 98
    keyK = 75

    downKeys :: [Int]
    downKeys = [keyS, arrowDown, numpad5, numpad2, keyK]

    initialize :: IO (IORef Keyboard)
    initialize = do
        keyboardRef <- IORef.newIORef $ Keyboard { left = False, right = False, up = False, down = False }
        registerKeyboardEvent keyboardRef Events.KeyDown True
        registerKeyboardEvent keyboardRef Events.KeyUp False
        return keyboardRef

    registerKeyboardEvent :: IORef Keyboard -> Events.KeyEvent -> Bool -> IO ()
    registerKeyboardEvent keyboardRef event isDown = do
        Events.onEvent DOM.document event $ \(KeyData keyCode _ _ _ _) -> updateKeyboard keyboardRef keyCode isDown
        return ()

    updateKeyboard :: IORef Keyboard -> Int -> Bool -> IO ()
    updateKeyboard keyboardRef keyCode isDown = do
        keyboard <- IORef.readIORef keyboardRef
        let updatedKeyboard = processKeyCode keyboard keyCode isDown
        IORef.writeIORef keyboardRef updatedKeyboard
        return ()

    processKeyCode :: Keyboard -> Int -> Bool -> Keyboard
    processKeyCode keyboard keyCode isDown
        | elem keyCode leftKeys  = keyboard { left = isDown }
        | elem keyCode rightKeys = keyboard { right = isDown }
        | elem keyCode upKeys    = keyboard { up = isDown }
        | elem keyCode downKeys  = keyboard { down = isDown }
        | otherwise              = keyboard