module Utils where

    clamp :: (Ord a) => a -> a -> a -> a
    clamp from to = max from . min to

    lerp :: (Num a) => a -> a -> a -> a
    lerp v0 v1 t = ((1 - t) * v0) + (t * v1)