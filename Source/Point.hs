{-# LANGUAGE NamedFieldPuns #-}

module Point
    ( Point(Point, x, y)
    , Point.magnitude
    , Point.normalize
    , Point.multiply
    , Point.clamp
    , Point.angle
    , Point.fromAngle
    , Point.subtract
    , Point.normal
    , Point.dot
    , Point.zero
    ) where

    import Utils

    data Point = Point
        { x :: Double
        , y :: Double
        } deriving (Show, Eq)

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

    fromAngle :: Double -> Point
    fromAngle angle = Point { x = cos angle, y = sin angle }

    subtract :: Point -> Point -> Point
    subtract Point{x = x0, y = y0} Point{x = x1, y = y1} = Point { x = x1 - x0, y = y1 - y0 }

    normal :: Point -> Point -> Point
    normal pointA pointB =
        let
            Point{x, y} = Point.subtract pointA pointB
        in
            Point { x = -y, y = x }

    dot :: Point -> Point -> Double
    dot Point{x = x0, y = y0} Point{x = x1, y = y1} = (x0 * x1) + (y0 * y1)

    zero :: Point
    zero = Point{ x = 0, y = 0 }