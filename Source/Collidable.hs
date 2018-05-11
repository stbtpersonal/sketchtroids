{-# LANGUAGE NamedFieldPuns #-}

module Collidable where

    import Rectangle
    import Resources

    class Collidable a where

        boundingBox :: a -> Resources -> Rectangle

        haveCollided :: Collidable b => a -> b -> Resources -> Bool
        haveCollided a b resources= 
            let
                boundingBoxA = boundingBox a resources
                boundingBoxB = boundingBox b resources
            in
                (Rectangle.right boundingBoxA >= Rectangle.left boundingBoxB) &&
                (Rectangle.left boundingBoxA <= Rectangle.right boundingBoxB) &&
                (Rectangle.bottom boundingBoxA >= Rectangle.top boundingBoxB) &&
                (Rectangle.top boundingBoxA <= Rectangle.bottom boundingBoxB)