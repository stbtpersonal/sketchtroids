module Foreign.Keyboard where

    import Haste
    import Haste.Foreign as Foreign

    isKeyDown :: String -> IO Bool
    isKeyDown = Foreign.ffi $ Haste.toJSString "KEYBOARD['isKeyDown']"