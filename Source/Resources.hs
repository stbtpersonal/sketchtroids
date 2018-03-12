module Resources where

    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas
    import Foreign.ImageLoader as ImageLoader

    data Resources = Resources { images :: [(String, Canvas.Bitmap)] }

    imageKeysToPaths :: [(String, String)]
    imageKeysToPaths = [ ("Spaceship", "Resources/Spaceship.png")
                       , ("Bullet", "Resources/Bullet.png")
                       ]

    loadResources :: IO ()
    loadResources = mapM_ loadResource imageKeysToPaths

    loadResource :: (String, String) -> IO ()
    loadResource keyToPath = do
        let path = snd keyToPath
        ImageLoader.loadImage path

    areResourcesLoaded :: IO Bool
    areResourcesLoaded = do
        results <- mapM isResourceLoaded imageKeysToPaths 
        return $ or results

    isResourceLoaded :: (String, String) -> IO Bool
    isResourceLoaded keyToPath = do
        let path = snd keyToPath
        ImageLoader.isImageLoaded path

    getResources :: IO Resources
    getResources = do
        keysToBitmaps <- mapM getResource imageKeysToPaths
        return Resources { images = keysToBitmaps }

    getResource :: (String, String) -> IO (String, Bitmap)
    getResource keyToPath = do
        let key = fst keyToPath
        let path = snd keyToPath
        bitmap <- ImageLoader.getLoadedImage path
        return (key, bitmap)