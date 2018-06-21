{-# LANGUAGE NamedFieldPuns #-}

module PreloadingMode
    ( PreloadingMode()
    , PreloadingMode.new
    ) where

    import Entity
    import Resources
    import LoadingMode
    import Data.Map as Map
    import Input

    data PreloadingMode = PreloadingMode

    new :: PreloadingMode
    new = PreloadingMode

    instance EntityClass PreloadingMode where

        load _ = LoadingMode.imageDefs

        update preloadingMode input@Input{resources} =
            let
                images = Resources.images resources
                loadedImageKeys = Map.keys images
                requiredImageKeys = Prelude.map fst LoadingMode.imageDefs
            in
                if and $ Prelude.map (\key -> elem key loadedImageKeys) requiredImageKeys
                    then Entity $ LoadingMode.new
                    else Entity $ preloadingMode
