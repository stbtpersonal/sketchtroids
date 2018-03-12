module Resources (loadResources, areResourcesLoaded, getResources, images) where

    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas
    import Foreign.ImageLoader as ImageLoader
    import Data.Map as Map
    import ResourceKeys

    data Resources = Resources { images :: Map ResourceKeys.ResourceKey Canvas.Bitmap }

    imageKeysToPaths :: [(ResourceKeys.ResourceKey, String)]
    imageKeysToPaths = [ (ResourceKeys.Spaceship, "Resources/Spaceship.png")
                       , (ResourceKeys.Bullet, "Resources/Bullet.png")
                       ]

    loadResources :: IO ()
    loadResources = mapM_ loadResource imageKeysToPaths

    loadResource :: (ResourceKeys.ResourceKey, String) -> IO ()
    loadResource (_, path) = ImageLoader.loadImage path

    areResourcesLoaded :: IO Bool
    areResourcesLoaded = do
        results <- mapM isResourceLoaded imageKeysToPaths 
        return $ or results

    isResourceLoaded :: (ResourceKeys.ResourceKey, String) -> IO Bool
    isResourceLoaded (_, path) = ImageLoader.isImageLoaded path

    getResources :: IO Resources
    getResources = do
        keysToBitmaps <- mapM getResource imageKeysToPaths
        let keysToBitmapsMap = Map.fromList keysToBitmaps
        return Resources { images = keysToBitmapsMap }

    getResource :: (ResourceKeys.ResourceKey, String) -> IO (ResourceKeys.ResourceKey, Bitmap)
    getResource (key, path) = do
        bitmap <- ImageLoader.getLoadedImage path
        return (key, bitmap)