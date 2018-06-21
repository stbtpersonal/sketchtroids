{-# LANGUAGE RecordWildCards #-}

module Keyboard
    ( Keyboard(Keyboard, left, right, up, down, action) 
    , Keyboard.initialize
    ) where

    import Data.IORef as IORef
    import Haste.DOM as DOM
    import Haste.Events as Events
    import Haste
    import Data.Set as Set

    data Keyboard = Keyboard
        { left :: Bool
        , right :: Bool
        , up :: Bool
        , down :: Bool
        , action :: Bool
        , keysDown :: Set Int
        }

    keyA = 65
    arrowLeft = 37
    numpad4 = 100
    keyJ = 74

    leftKeys :: Set Int
    leftKeys = Set.fromList [keyA, arrowLeft, numpad4, keyJ]

    keyD = 68
    arrowRight = 39
    numpad6 = 102
    keyL = 76

    rightKeys :: Set Int
    rightKeys = Set.fromList [keyD, arrowRight, numpad6, keyL]

    keyW = 87
    arrowUp = 38
    numpad8 = 104
    keyI = 73

    upKeys :: Set Int
    upKeys = Set.fromList [keyW, arrowUp, numpad8, keyI]

    keyS = 83
    arrowDown = 40
    numpad5 = 101
    numpad2 = 98
    keyK = 75

    downKeys :: Set Int
    downKeys = Set.fromList [keyS, arrowDown, numpad5, numpad2, keyK]

    keySpace = 32
    keyEnter = 13
    keyCtrl = 17

    actionKeys :: Set Int
    actionKeys = Set.fromList [keySpace, keyEnter, Keyboard.keyCtrl]

    initialize :: IO (IORef Keyboard)
    initialize = do
        keyboardRef <- IORef.newIORef $ Keyboard { left = False, right = False, up = False, down = False, action = False, keysDown = Set.empty }
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
    processKeyCode keyboard@Keyboard{..} keyCode isDown =
        let
            updatedKeysDown = if isDown then Set.insert keyCode keysDown else Set.delete keyCode keysDown
            isKeyDown keys = not (Set.null $ Set.intersection updatedKeysDown keys)
            updatedLeft = isKeyDown leftKeys
            updatedRight = isKeyDown rightKeys
            updatedUp = isKeyDown upKeys
            updatedDown = isKeyDown downKeys
            updatedAction = isKeyDown actionKeys
        in
            Keyboard
                { left = updatedLeft
                , right = updatedRight
                , up = updatedUp
                , down = updatedDown
                , action = updatedAction
                , keysDown = updatedKeysDown
                }