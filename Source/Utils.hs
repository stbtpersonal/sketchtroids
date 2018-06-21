module Utils
    ( Utils.clamp
    , Utils.wrap
    , Utils.lerp
    , Utils.writeLog
    , Utils.unsafeWriteLog
    , Utils.splits
    ) where

    import Haste
    import Haste.Foreign as Foreign
    import System.IO.Unsafe as Unsafe
    import System.Random as Random

    clamp :: (Ord a) => a -> a -> a -> a
    clamp from to = max from . min to

    wrap :: Double -> Double -> Double -> Double
    wrap from to value
        | value < from = to - (from - value)
        | value > to   = from + (value - to)
        | otherwise    = value

    lerp :: (Num a) => a -> a -> a -> a
    lerp v0 v1 t = ((1 - t) * v0) + (t * v1)

    writeLog :: String -> IO Double
    writeLog = Foreign.ffi $ Haste.toJSString "console.log"

    unsafeWriteLog :: String -> Double
    unsafeWriteLog string = Unsafe.unsafePerformIO $ Utils.writeLog string

    splits :: Random.StdGen -> [Random.StdGen]
    splits origin =
        let
            (nextGenerator, nextOrigin) = Random.split origin
        in
            nextGenerator : splits nextOrigin