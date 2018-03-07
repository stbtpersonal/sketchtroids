module Game where

    import Haste.Graphics.Canvas as Canvas
    import Entity

    data Game = Game { canvas :: Canvas, timestamp :: Double, entities :: [Entity] }

    new :: Canvas -> Game
    new canvas = Game { canvas = canvas, timestamp = 0, entities = [] }