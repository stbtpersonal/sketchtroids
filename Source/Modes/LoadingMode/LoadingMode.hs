{-# LANGUAGE RecordWildCards #-}

module Modes.LoadingMode.LoadingMode where

    import Entity
    import Resources
    import Modes.GameMode.GameMode as GameMode
    import Data.Map as Map
    import Input

    data LoadingMode = LoadingMode

    new :: LoadingMode
    new = LoadingMode

    instance EntityClass LoadingMode where

        load _ = GameMode.imageKeysToPaths

        update loadingMode input@Input{..} =
            let
                images = Resources.images resources
                loadedImageKeys = Map.keys images
                requiredImageKeys = Prelude.map fst GameMode.imageKeysToPaths
            in
                if loadedImageKeys == requiredImageKeys
                    then Entity $ GameMode.new
                    else Entity $ loadingMode
