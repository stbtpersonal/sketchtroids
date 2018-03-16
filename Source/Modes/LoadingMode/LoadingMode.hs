{-# LANGUAGE NamedFieldPuns #-}

module Modes.LoadingMode.LoadingMode where

    import Entity
    import Resources
    import Modes.GameMode.GameMode as GameMode
    import Data.Map as Map
    import Input
    import ResourceKey

    data LoadingMode = LoadingMode

    new :: LoadingMode
    new = LoadingMode

    imageResources :: Resources.ResourceKeysToPaths
    imageResources = [ (ResourceKey "Loading", "Resources/Loading.png") ]

    instance EntityClass LoadingMode where

        load _ = GameMode.imageResources

        update loadingMode input@Input{resources} =
            let
                images = Resources.images resources
                loadedImageKeys = Map.keys images
                requiredImageKeys = Prelude.map fst GameMode.imageResources
            in
                if and $ Prelude.map (\key -> elem key loadedImageKeys) requiredImageKeys
                    then Entity $ GameMode.new
                    else Entity $ loadingMode
