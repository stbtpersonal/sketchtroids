{-# LANGUAGE RecordWildCards #-}

module Resources (Resources, Resources.empty, ImageKeysToPaths, loadResources, appendResources, images) where

    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas
    import Foreign.ImageLoader as ImageLoader
    import Data.Map as Map
    import ResourceKeys
    import Control.Monad

    data Resources = Resources { images :: Map ResourceKeys.ResourceKey Canvas.Bitmap }

    empty :: Resources
    empty = Resources { images = Map.empty }

    type ImageKeysToPaths = [(ResourceKeys.ResourceKey, String)]

    loadResources :: ImageKeysToPaths -> IO ()
    loadResources imageKeysToPaths = mapM_ loadResource imageKeysToPaths

    loadResource :: (ResourceKeys.ResourceKey, String) -> IO ()
    loadResource (_, path) = ImageLoader.loadImage path

    appendResources :: ImageKeysToPaths -> Resources -> IO Resources
    appendResources imageKeysToPaths resources@Resources{..} = do
        let missingImageKeysToPaths = excludeLoadedResources imageKeysToPaths resources
        availableImageKeysToPaths <- excludeUnavailableResources missingImageKeysToPaths
        keysToBitmaps <- mapM getResource availableImageKeysToPaths
        let keysToBitmapsMap = Map.fromList keysToBitmaps
        return Resources { images = Map.union images keysToBitmapsMap }

    excludeLoadedResources :: ImageKeysToPaths -> Resources -> ImageKeysToPaths
    excludeLoadedResources imageKeysToPaths resources@Resources{..} =
        let
            existingKeys = Map.keys images
        in
            Prelude.filter (\(key, _) -> not $ elem key existingKeys) imageKeysToPaths

    excludeUnavailableResources :: ImageKeysToPaths -> IO ImageKeysToPaths
    excludeUnavailableResources imageKeysToPaths = filterM (\(_, path) -> ImageLoader.isImageLoaded path) imageKeysToPaths

    getResource :: (ResourceKeys.ResourceKey, String) -> IO (ResourceKeys.ResourceKey, Bitmap)
    getResource (key, path) = do
        bitmap <- ImageLoader.getLoadedImage path
        return (key, bitmap)