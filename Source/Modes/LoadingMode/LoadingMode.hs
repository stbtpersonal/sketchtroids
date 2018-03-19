{-# LANGUAGE NamedFieldPuns #-}

module Modes.LoadingMode.LoadingMode(
    LoadingMode(LoadingMode),
    Modes.LoadingMode.LoadingMode.new,
    Modes.LoadingMode.LoadingMode.imageDefs) where

    import Entity
    import Resources
    import Modes.GameMode.GameMode as GameMode
    import Data.Map as Map
    import Input
    import CommonEntities.Background as Background
    import CommonEntities.Fps as Fps
    import Modes.LoadingMode.Entities.Spinner as Spinner

    data LoadingMode = LoadingMode { children :: [Entity] }

    new :: LoadingMode
    new = LoadingMode { children = [ Entity $ Background.new
                                   , Entity $ Fps.new
                                   , Entity $ Spinner.new
                                   ] }

    imageDefs :: [Resources.ResourceDef]
    imageDefs =
        let
            loadingMode@LoadingMode{children} = Modes.LoadingMode.LoadingMode.new
        in
            Entity.loadAll children

    instance EntityClass LoadingMode where

        load loadingMode@LoadingMode{children} = GameMode.imageDefs

        update loadingMode@LoadingMode{children} input@Input{resources} =
            let
                images = Resources.images resources
                loadedImageKeys = Map.keys images
                requiredImageKeys = Prelude.map fst GameMode.imageDefs

                updatedChildren = Entity.updateAll children input
            in
                if and $ Prelude.map (\key -> elem key loadedImageKeys) requiredImageKeys
                    then Entity $ loadingMode { children = updatedChildren }
                    else Entity $ loadingMode { children = updatedChildren }

        render LoadingMode{children} resources = Entity.renderAll children resources
