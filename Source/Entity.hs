{-# LANGUAGE ExistentialQuantification #-}

module Entity where

    import Resources
    import Input
    import Haste.Graphics.Canvas as Canvas

    class EntityClass a where

        load :: a -> Resources.ResourceKeysToPaths
        load _ = []

        update :: a -> Input -> Entity
        update a _ = Entity a

        render :: a -> Canvas.Picture ()
        render _ = Canvas.setStrokeColor $ Canvas.RGB 0 0 0

    data Entity = forall a . EntityClass a => Entity a

    instance EntityClass Entity where
        load (Entity a) = Entity.load a
        update (Entity a) = Entity.update a
        render (Entity a) = Entity.render a

    updateAll :: [Entity] -> Input -> [Entity]
    updateAll entities input = map (\entity -> Entity.update entity input) entities

    renderAll :: [Entity] -> Canvas.Picture ()
    renderAll entities = mapM_ Entity.render entities