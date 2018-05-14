{-# LANGUAGE NamedFieldPuns #-}

module Resources
    ( ResourceKey(ResourceKey)
    , ResourceDef
    , BitmapData(BitmapData, _bitmap, _width, _height, _collisionPolygon)
    , Resources(Resources, images)
    , initialize
    , loadImages
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

    type ResourceDef = (ResourceKey, String)

    data BitmapData = BitmapData
        { _bitmap :: Bitmap
        , _width :: Double
        , _height :: Double
        , _collisionPolygon :: CollisionPolygon
        }

    data Resources = Resources
        { images :: Map ResourceKey BitmapData
        , requestedKeys :: Set ResourceKey
        }

    initialize :: IO (IORef Resources)
    initialize = IORef.newIORef $ Resources { images = Map.empty, requestedKeys = Set.empty }

    loadImages :: IORef Resources -> [ResourceDef] -> IO ()
    loadImages resourcesRef keysToPaths = do
        resources@Resources{requestedKeys} <- IORef.readIORef resourcesRef
        let unrequestedKeysToPaths = Prelude.filter (\(key, _) -> Set.notMember key requestedKeys) keysToPaths
        mapM_ (loadImage resourcesRef) unrequestedKeysToPaths

    loadImage :: IORef Resources -> ResourceDef -> IO ()
    loadImage resourcesRef (key, path) = do
        resources@Resources{requestedKeys} <- IORef.readIORef resourcesRef
        let updatedRequestedKeys = Set.insert key requestedKeys
        IORef.writeIORef resourcesRef $ resources { requestedKeys = updatedRequestedKeys }

        bitmap <- Canvas.loadBitmap $ Haste.toJSString path
        let bitmapElement = DOM.elemOf bitmap
        Events.onEvent bitmapElement Events.Load $ (\_ -> finishLoadingImage resourcesRef key bitmap)
        return ()

    finishLoadingImage :: IORef Resources -> ResourceKey -> Canvas.Bitmap -> IO ()
    finishLoadingImage resourcesRef key bitmap = do
        sprite <- buildBitmapData bitmap
        resources@Resources{images} <- IORef.readIORef resourcesRef
        let updatedImages = Map.insert key sprite images
        IORef.writeIORef resourcesRef $ resources { images = updatedImages }
        return ()

    buildBitmapData :: Canvas.Bitmap -> IO BitmapData
    buildBitmapData bitmap = do
        let bitmapElement = DOM.elemOf bitmap
        widthProperty <- DOM.getProp bitmapElement "width"
        heightProperty <- DOM.getProp bitmapElement "height"
        let width = read widthProperty
        let height = read heightProperty
        collisionPolygon <- CollisionPolygon.build bitmap width height 
        return BitmapData
            { _bitmap = bitmap
            , _width = width
            , _height = height
            , _collisionPolygon = collisionPolygon
            }