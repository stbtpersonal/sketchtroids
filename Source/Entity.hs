{-# LANGUAGE ExistentialQuantification #-}

module Entity where

    import Input
    import Haste.Graphics.Canvas as Canvas

    class EntityClass a where
        update :: a -> Input -> Entity
        render :: a -> Canvas.Picture ()

    data Entity = forall a . EntityClass a => Entity a

    instance EntityClass Entity where
        update (Entity a) = Entity.update a
        render (Entity a) = Entity.render a

    updateAll :: [Entity] -> Input -> [Entity]
    updateAll entities input = map (\entity -> Entity.update entity input) entities

    renderAll :: [Entity] -> Canvas.Picture ()
    renderAll entities = mapM_ Entity.render entities