module Foreign.ImageLoader (loadImage, isImageLoaded, getLoadedImage) where

    import Haste
    import Haste.DOM as DOM
    import Haste.Graphics.Canvas as Canvas
    import Haste.Foreign as Foreign

    loadImage :: String -> IO ()
    loadImage = Foreign.ffi $ Haste.toJSString "IMAGE_LOADER['loadImage']"

    isImageLoaded :: String -> IO Bool
    isImageLoaded = Foreign.ffi $ Haste.toJSString "IMAGE_LOADER['isImageLoaded']"

    getLoadedImageElement :: String -> IO DOM.Elem
    getLoadedImageElement = Foreign.ffi $ Haste.toJSString "IMAGE_LOADER['getLoadedImage']"

    getLoadedImage :: String -> IO Canvas.Bitmap
    getLoadedImage path = do
        element <- getLoadedImageElement path
        bitmap <- Canvas.loadBitmap element
        return bitmap

