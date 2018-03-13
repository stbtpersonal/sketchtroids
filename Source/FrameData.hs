module FrameData where

    import Resources
    import Haste.Graphics.Canvas as Canvas
    import Entity
    
    data FrameData = FrameData { canvas :: Canvas, timestamp :: Double, resources :: Resources, mode :: Entity }