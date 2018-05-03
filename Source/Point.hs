{-# LANGUAGE NamedFieldPuns #-}

module Point(
    Point(Point, x, y),
    magnitude,
    normalize,
    multiply,
    Point.clamp,
    angle
    ) where

    import Utils

    data Point = Point { x :: Double, y :: Double } deriving Show

    magnitude :: Point -> Double
    magnitude Point{x, y} = sqrt (x * x + y * y)

    normalize :: Point -> Point
    normalize point@Point{x, y} = 
        let
            pointMagnitude = magnitude point
        in
            if pointMagnitude == 0 then point else Point { x = x / pointMagnitude, y = y / pointMagnitude }

    multiply :: Double -> Point -> Point
    multiply pointMagnitude Point{x, y} = Point { x = x * pointMagnitude, y = y * pointMagnitude }

    clamp :: Double -> Double -> Point -> Point
    clamp minMagnitude maxMagnitude point = 
        let
            pointMagnitude = magnitude point
            clampedMagnitude = Utils.clamp minMagnitude maxMagnitude pointMagnitude
            normalizedPoint = normalize point
            clampedPoint = multiply clampedMagnitude normalizedPoint
        in
            clampedPoint

    angle :: Point -> Double
    angle Point{x, y} = atan2 y x