module Utils where

    import Haste
    import Haste.Foreign as Foreign
    import System.IO.Unsafe as Unsafe

    clamp :: (Ord a) => a -> a -> a -> a
    clamp from to = max from . min to

    lerp :: (Num a) => a -> a -> a -> a
    lerp v0 v1 t = ((1 - t) * v0) + (t * v1)

    writeLog :: String -> IO Double
    writeLog = Foreign.ffi $ Haste.toJSString "console.log"

    unsafeWriteLog :: String -> Double
    unsafeWriteLog string = Unsafe.unsafePerformIO $ Utils.writeLog string