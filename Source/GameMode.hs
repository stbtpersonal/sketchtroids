{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module GameMode
    ( imageDefs
    , GameMode(GameMode)
    , GameMode.new
    ) where

    import Entity
    import Background
    import Fps
    import Point
    import Input
    import Resources
    import Ship
    import Asteroid
    import Collidable
    import Utils
    import PressToStartText (PressToStartText, new)
    import Sprite (update, isEnabled, setEnabled)

    data GameMode = GameMode
        { background :: Background
        , ship :: Ship
        , asteroid :: Asteroid
        , fps :: Fps
        , pressToStartText :: PressToStartText
        }

    new :: GameMode
    new = GameMode
        { background = Background.new
        , ship = Ship.new
        , asteroid = Asteroid.new
        , fps = Fps.new
        , pressToStartText = PressToStartText.new
        }

    children :: GameMode -> [Entity]
    children GameMode{background, ship, asteroid, fps, pressToStartText} = [Entity background, Entity ship, Entity asteroid, Entity fps, Entity pressToStartText]

    imageDefs :: [Resources.ResourceDef]
    imageDefs = Entity.loadAll $ children GameMode.new

    instance EntityClass GameMode where

        update gameMode@GameMode{ship, asteroid, fps, pressToStartText} input@Input{resources} = 
            let
                fps' = Fps.update' fps input

                pressToStartText' = Sprite.update pressToStartText input
                ship' = if (not $ Sprite.isEnabled ship) && (not $ Sprite.isEnabled pressToStartText')
                    then Sprite.setEnabled ship True
                    else ship
                asteroid' = if (not $ Sprite.isEnabled asteroid) && (not $ Sprite.isEnabled pressToStartText')
                    then Sprite.setEnabled asteroid True
                    else asteroid

                ship'' = Ship.update' ship' input
                asteroid'' = Asteroid.update' asteroid' input

                haveCollided = Collidable.haveCollided ship'' asteroid'' resources
                !kaka = Utils.unsafeWriteLog $ "AAA " ++ (show haveCollided)
            in
                Entity $ gameMode
                    { ship = ship''
                    , asteroid = asteroid''
                    , fps = fps'
                    , pressToStartText = pressToStartText'
                    }

        render gameMode resources = Entity.renderAll (children gameMode) resources