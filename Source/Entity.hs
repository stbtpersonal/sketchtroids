{-# LANGUAGE ExistentialQuantification #-}

module Entity where

    import Haste.Graphics.Canvas as Canvas

    class EntityClass a where
        update :: a -> Double -> Entity
        render :: a -> Canvas.Picture ()

    data Entity = forall a . EntityClass a => Entity a

    instance EntityClass Entity where
        update (Entity a) = Entity.update a
        render (Entity a) = Entity.render a

    updateAll :: [Entity] -> Double -> [Entity]
    updateAll entities deltaTime = map (\entity -> Entity.update entity deltaTime) entities

    renderAll :: [Entity] -> Canvas.Picture ()
    renderAll entities = mapM_ Entity.render entities