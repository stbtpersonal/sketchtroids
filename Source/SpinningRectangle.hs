module SpinningRectangle where

    import Point
    import Entity
    import Haste.Graphics.Canvas as Canvas

    data SpinningRectangle = SpinningRectangle { rotation :: Double, speed :: Double, position :: Point.Point }

    new :: SpinningRectangle
    new = SpinningRectangle { rotation = 0, speed = 0, position = Point { x = 0, y = 0 } }

    instance EntityClass SpinningRectangle where

        update spinningRectangle deltaTime =
            let
                rotation = SpinningRectangle.rotation spinningRectangle
                speed = SpinningRectangle.speed spinningRectangle
                updatedRotation = rotation + (speed * deltaTime)
            in
                Entity $ spinningRectangle { rotation = updatedRotation }

        render spinningRectangle =
            let
                rotation = SpinningRectangle.rotation spinningRectangle
                position = SpinningRectangle.position spinningRectangle

                shape = Canvas.rect (-25, -25) (25, 25)
                stroked = Canvas.stroke shape
                colored = Canvas.color (Canvas.RGB 255 0 0) stroked
                rotated = Canvas.rotate rotation colored
                translated = Canvas.translate (Point.x position, Point.y position) rotated
            in
                translated