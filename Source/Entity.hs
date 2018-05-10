{-# LANGUAGE ExistentialQuantification #-}

module Entity where

    import Resources
    import Input
    import Haste.Graphics.Canvas as Canvas

    class EntityClass a where

        load :: a -> [Resources.ResourceDef]
        load _ = []

        update :: a -> Input -> Entity
        update a _ = Entity a

        render :: a -> Resources -> Canvas.Picture ()
        render _ _ = Canvas.setStrokeColor $ Canvas.RGB 0 0 0

    data Entity = forall a . EntityClass a => Entity a

    instance EntityClass Entity where
        load (Entity a) = Entity.load a
        update (Entity a) = Entity.update a
        render (Entity a) = Entity.render a

    loadAll :: [Entity] -> [Resources.ResourceDef]
    loadAll entities = concat $ map Entity.load entities

    updateAll :: [Entity] -> Input -> [Entity]
    updateAll entities input = zipWith ($) (map (\entity -> Entity.update entity) entities) (Input.randomize input)

    renderAll :: [Entity] -> Resources -> Canvas.Picture ()
    renderAll entities resources = mapM_ (\entity -> Entity.render entity resources) entities