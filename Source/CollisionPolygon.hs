{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module CollisionPolygon where

    import Point (Point(Point, x, y))
    import Haste.Graphics.Canvas as Canvas (Bitmap)
    import Utils (unsafeWriteLog)

    data CollisionPolygon = CollisionPolygon { points :: [Point] }

    build :: Bitmap -> Double -> Double -> CollisionPolygon
    build bitmap width height =
        let
            !kaka = Utils.unsafeWriteLog ("AAAAAAAAAAAAAA!!!! " ++ (show width) ++ " " ++ (show height))
        in
            CollisionPolygon { points = [] }