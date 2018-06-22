{-# LANGUAGE NamedFieldPuns #-}

module Resources
    ( Resources(Resources, images, collisions, loadedKeys)
    , Resources.ResourceDef
    , Resources.ResourceType(Image, Collision)
    , Resources.BitmapData(BitmapData, _bitmap, _width, _height)
    , Resources.CollisionData(CollisionData, _collisionPolygon)
    , Resources.ResourceKey(ResourceKey)
    , Resources.initialize
    , Resources.loadResources
    ) where

    import Data.Map as Map
    import Data.Set as Set
    import Data.IORef as IORef
    import Haste.Graphics.Canvas as Canvas
    import Haste
    import Haste.Events as Events
    import Haste.DOM as DOM
    import CollisionPolygon

    newtype ResourceKey = ResourceKey String deriving (Eq, Ord, Show)

    data ResourceType = Image | Collision

    type ResourceDef = (ResourceKey, ResourceType, String)

    data BitmapData = BitmapData
        { _bitmap :: Bitmap
        , _width :: Double
        , _height :: Double
        }

    data CollisionData = CollisionData
        { _collisionPolygon :: CollisionPolygon
        }

    data Resources = Resources
        { images :: Map ResourceKey BitmapData
        , collisions :: Map ResourceKey CollisionData
        , requestedKeys :: Set ResourceKey
        , loadedKeys :: Set ResourceKey
        }

    initialize :: IO (IORef Resources)
    initialize = IORef.newIORef $ Resources { images = Map.empty, collisions = Map.empty, requestedKeys = Set.empty, loadedKeys = Set.empty }

    loadResources :: IORef Resources -> [ResourceDef] -> IO ()
    loadResources resourcesRef resourceDefs = do
        resources@Resources{requestedKeys} <- IORef.readIORef resourcesRef
        let unrequestedResourceDefs = Prelude.filter (\(key, _, _) -> Set.notMember key requestedKeys) resourceDefs
        mapM_ (loadResource resourcesRef) unrequestedResourceDefs

    loadResource :: IORef Resources -> ResourceDef -> IO ()
    loadResource resourcesRef (key, resourceType, path) = do
        resources@Resources{requestedKeys} <- IORef.readIORef resourcesRef
        let updatedRequestedKeys = Set.insert key requestedKeys
        IORef.writeIORef resourcesRef $ resources { requestedKeys = updatedRequestedKeys }

        bitmap <- Canvas.loadBitmap $ Haste.toJSString path
        let bitmapElement = DOM.elemOf bitmap
        Events.onEvent bitmapElement Events.Load $ (\_ -> (finishFunction resourceType) resourcesRef key bitmap)
        return ()

    finishFunction :: ResourceType -> (IORef Resources -> ResourceKey -> Canvas.Bitmap -> IO ())
    finishFunction Image = finishLoadingImage
    finishFunction Collision = finishLoadingCollision

    finishLoadingImage :: IORef Resources -> ResourceKey -> Canvas.Bitmap -> IO ()
    finishLoadingImage resourcesRef key bitmap = do
        image <- buildBitmapData bitmap
        resources@Resources{images, loadedKeys} <- IORef.readIORef resourcesRef
        let updatedImages = Map.insert key image images
        let updatedLoadedKeys = Set.insert key loadedKeys
        IORef.writeIORef resourcesRef $ resources { images = updatedImages, loadedKeys = updatedLoadedKeys }
        return ()

    buildBitmapData :: Canvas.Bitmap -> IO BitmapData
    buildBitmapData bitmap = do
        let bitmapElement = DOM.elemOf bitmap
        widthProperty <- DOM.getProp bitmapElement "width"
        heightProperty <- DOM.getProp bitmapElement "height"
        let width = read widthProperty
        let height = read heightProperty 
        return BitmapData
            { _bitmap = bitmap
            , _width = width
            , _height = height
            }

    finishLoadingCollision :: IORef Resources -> ResourceKey -> Canvas.Bitmap -> IO ()
    finishLoadingCollision resourcesRef key bitmap = do
        BitmapData{_bitmap, _width, _height} <- buildBitmapData bitmap
        collisionPolygon <- CollisionPolygon.build _bitmap _width _height
        let collisionData = CollisionData{_collisionPolygon = collisionPolygon}
        resources@Resources{collisions, loadedKeys} <- IORef.readIORef resourcesRef
        let updatedCollisions = Map.insert key collisionData collisions
        let updatedLoadedKeys = Set.insert key loadedKeys
        IORef.writeIORef resourcesRef $ resources { collisions = updatedCollisions, loadedKeys = updatedLoadedKeys }
        return ()