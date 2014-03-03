
module Game.GameState
       (
         GameState (..)
       , mkGameState
       ) where


import Game.Basics
import Game.Ship
import Game.Walls

import Data.Word (Word32)
import qualified Graphics.UI.SDL as SDL

data GameState = GameState {
    gShip  :: Ship
  , gWalls :: Walls
  } deriving (Show)

mkGameState :: Config -> GameState
mkGameState conf = GameState {
    gShip  = mkShip conf
  , gWalls = mkWalls conf
  }

instance Renderable GameState where
  render s c = do
    screen <- SDL.getVideoSurface
  
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
      SDL.fillRect screen Nothing
    render (gShip s) c
    render (gWalls s) c
    
    SDL.flip screen
    return ()
