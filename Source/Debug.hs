{-# LANGUAGE NamedFieldPuns #-}

module Debug where

    import Haste.Foreign as Foreign (ffi)
    import Haste (toJSString)

    isDebugEnabled :: IO Bool
    isDebugEnabled = Foreign.ffi $ Haste.toJSString "DEBUG['isDebugEnabled']"