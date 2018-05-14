{-# LANGUAGE NamedFieldPuns #-}

module CollisionPolygon where

    import Point (Point(Point, x, y))
    import Haste.Graphics.Canvas as Canvas (Bitmap, Canvas, draw, render)
    import Utils (writeLog)
    import Haste.DOM as DOM (newElem, setProp, fromElem)
    import Constants (nativeWidth, nativeHeight)
    import Haste.Foreign as Foreign (ffi)
    import Haste (toJSString)

    data CollisionPolygon = CollisionPolygon { points :: [Point] }

    build :: Bitmap -> Double -> Double -> IO CollisionPolygon
    build bitmap width height = do
        jsPoints <- jsBuild bitmap width height
        Utils.writeLog $ "GGGGG " ++ (show jsPoints)
        return $ CollisionPolygon { points = [] }

    jsBuild :: Bitmap -> Double -> Double -> IO [Int]
    jsBuild = Foreign.ffi $ Haste.toJSString "COLLISION_POLYGON['build']"