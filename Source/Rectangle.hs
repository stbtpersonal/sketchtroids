{-# LANGUAGE NamedFieldPuns #-}

module Rectangle where

    import Point

    data Rectangle = Rectangle
        { topLeft :: Point
        , bottomRight :: Point
        }

    left :: Rectangle -> Double
    left Rectangle{topLeft} = Point.x topLeft

    right :: Rectangle -> Double
    right Rectangle{bottomRight} = Point.x bottomRight

    top :: Rectangle -> Double
    top Rectangle{topLeft} = Point.y topLeft

    bottom :: Rectangle -> Double
    bottom Rectangle{bottomRight} = Point.y bottomRight

    width :: Rectangle -> Double
    width Rectangle{topLeft, bottomRight} = 
        let
            left = Point.x topLeft
            right = Point.x bottomRight
        in
            right - left

    height :: Rectangle -> Double
    height Rectangle{topLeft, bottomRight} =
        let
            top = Point.y topLeft
            bottom = Point.y bottomRight
        in
            bottom - top