
module Game.GameState
       (
         GameState (..)
       , mkGameState
       ) where


import Game.Types
import Game.Ship
import qualified Graphics.UI.SDL as SDL

data GameState = GameState {
  gShip :: Ship
  } deriving (Show)

mkGameState :: GameState
mkGameState = GameState {
  gShip = mkShip
  }

instance Renderable GameState where
  render s c = do
    screen <- SDL.getVideoSurface
  
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
      SDL.fillRect screen Nothing
    render (gShip s) c
    
    SDL.flip screen
    return ()
