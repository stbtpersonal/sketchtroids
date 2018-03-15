module FrameData where

    import Resources
    import Haste.Graphics.Canvas as Canvas
    import Entity
    import Data.IORef as IORef
    import Keyboard
    
    data FrameData = FrameData { canvas :: Canvas, timestamp :: Double, resources :: Resources, keyboardRef :: IORef Keyboard, mode :: Entity }