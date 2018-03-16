module ResourceKey where

    data ResourceKey = Spaceship
                     | Bullet
                     | Big
                     deriving (Eq, Ord, Show)