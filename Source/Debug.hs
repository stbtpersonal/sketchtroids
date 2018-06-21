{-# LANGUAGE NamedFieldPuns #-}

module Debug
    ( Debug.isDebugEnabled
    ) where

    import Haste.Foreign as Foreign
    import Haste

    isDebugEnabled :: IO Bool
    isDebugEnabled = Foreign.ffi $ Haste.toJSString "DEBUG['isDebugEnabled']"