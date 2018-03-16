{-# LANGUAGE NamedFieldPuns #-}

module Resources (ResourceKeysToPaths, Resources(images), initialize, loadImages) where

    import Data.Map as Map
    import ResourceKey
    import Data.Set as Set
    import Data.IORef as IORef
    import Haste.Graphics.Canvas as Canvas
    import Haste
    import Haste.Events as Events
    import Haste.DOM as DOM

    type ResourceKeysToPaths = [(ResourceKey, String)]

    data Resources = Resources { images :: Map ResourceKey Canvas.Bitmap, requestedKeys :: Set ResourceKey }

    initialize :: IO (IORef Resources)
    initialize = IORef.newIORef $ Resources { images = Map.empty, requestedKeys = Set.empty }

    loadImages :: IORef Resources -> ResourceKeysToPaths -> IO ()
    loadImages resourcesRef keysToPaths = do
        resources@Resources{requestedKeys} <- IORef.readIORef resourcesRef
        let unrequestedKeysToPaths = Prelude.filter (\(key, _) -> Set.notMember key requestedKeys) keysToPaths
        mapM_ (loadImage resourcesRef) unrequestedKeysToPaths

    loadImage :: IORef Resources -> (ResourceKey, String) -> IO ()
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
        resources@Resources{images} <- IORef.readIORef resourcesRef
        let updatedImages = Map.insert key bitmap images
        IORef.writeIORef resourcesRef $ resources { images = updatedImages }
        return ()