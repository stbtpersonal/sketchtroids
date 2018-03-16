module ResourceKey where

    newtype ResourceKey = ResourceKey String deriving (Eq, Ord, Show)