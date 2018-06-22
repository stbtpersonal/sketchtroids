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
    import PleaseWaitText

    data PreloadingMode = PreloadingMode

    new :: PreloadingMode
    new = PreloadingMode

    pleaseWaitText :: PleaseWaitText
    pleaseWaitText = PleaseWaitText.new

    instance EntityClass PreloadingMode where

        load _ = LoadingMode.resourceDefs

        update preloadingMode input@Input{resources} =
            let
                Resources{loadedKeys} = resources
                requiredKeys = Prelude.map (\(key, _, _) -> key) LoadingMode.resourceDefs
            in
                if and $ Prelude.map (\key -> elem key loadedKeys) requiredKeys
                    then Entity $ LoadingMode.new
                    else Entity $ preloadingMode

        render _ input = Entity.render (Entity pleaseWaitText) input