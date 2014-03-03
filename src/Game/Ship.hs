
module Game.Ship
       (
         Ship (..),
         mkShip
       ) where

import Game.Basics
import qualified Graphics.UI.SDL as SDL

data Ship = Ship {
    sPosition :: (Int, Int)
  , sSpeed    :: Int  
  } deriving (Show)

mkShip :: Config -> Ship
mkShip Config{cWorldSize} = Ship {
    sPosition = ((fst cWorldSize) `div` 2, (snd cWorldSize) `div` 2)
  , sSpeed    = 1
  }

instance Renderable Ship where
  render Ship{sPosition} Config{cCellSize} = do
    screen <- SDL.getVideoSurface

    let (x, y) = (cCellSize * (fst sPosition), cCellSize * (snd sPosition))
              
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
      SDL.fillRect screen (Just $ SDL.Rect x y cCellSize cCellSize)

    return ()

instance Movable Ship where
  move Ship{sPosition,sSpeed} d = 
    let (x, y) = sPosition
    in
     Ship { sPosition = case d of
               ToLeft  -> (x - sSpeed, y)
               ToRight -> (x + sSpeed, y)
               ToUp    -> (x, y - sSpeed)
               ToDown  -> (x, y + sSpeed)
          , sSpeed = sSpeed
          }
