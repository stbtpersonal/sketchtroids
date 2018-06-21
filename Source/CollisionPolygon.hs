{-# LANGUAGE NamedFieldPuns #-}

module CollisionPolygon
    ( CollisionPolygon(CollisionPolygon, points)
    , CollisionPolygon.build
    ) where

    import Point
    import Haste.Graphics.Canvas as Canvas hiding (Point)
    import Haste.Foreign as Foreign
    import Haste

    data CollisionPolygon = CollisionPolygon { points :: [Point] }

    build :: Bitmap -> Double -> Double -> IO CollisionPolygon
    build bitmap width height = do
        jsPoints <- jsBuild bitmap width height
        return $ CollisionPolygon { points = getPointsFromFlatList jsPoints }

    jsBuild :: Bitmap -> Double -> Double -> IO [Double]
    jsBuild = Foreign.ffi $ Haste.toJSString "COLLISION_POLYGON['build']"

    getPointsFromFlatList :: [Double] -> [Point]
    getPointsFromFlatList [] = []
    getPointsFromFlatList (a : b : rest) = Point { x = a, y = b } : getPointsFromFlatList rest