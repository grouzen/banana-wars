{-# LANGUAGE RankNTypes #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Graphics.UI.SDL as SDL
import Data.Word (Word32)
import Control.Monad

-- Basic constants
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

frameRate :: Int
frameRate = 60


class Colliding a where
  collide :: a -> b -> Bool

class Moving a where
  move :: a -> Direction -> a

class Rendering a where
  render :: a -> IO ()

data Direction = ToLeft
               | ToRight
               | ToUp
               | ToDown

data Ship = Ship {
    position :: (Int, Int)
  , size     :: (Int, Int)
  } deriving (Show)


instance Moving Ship where
  move s d = 
    let (x, y) = position s
    in
     Ship {
       position = case d of
         ToLeft  -> (x - 20, y)
         ToRight -> (x + 20, y)
         ToUp    -> (x, y - 20)
         ToDown  -> (x, y + 20)
       , size = size s
       }

instance Rendering GameState where
  render s = do
    screen <- SDL.getVideoSurface
  
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 >>=
      SDL.fillRect screen Nothing
    render $ ship s
    
    SDL.flip screen
    return ()

instance Rendering Ship where
  render Ship{position,size} = do
    screen <- SDL.getVideoSurface
  
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
      SDL.fillRect screen (Just $ SDL.Rect (fst position) (snd position) (fst size) (snd size))

    return ()


data GameState = GameState {
  ship :: Ship
  } deriving (Show)


main :: IO ()
main = do
  screen  <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.SWSurface]
  sources <- (,) <$> newAddHandler <*> newAddHandler
  network <- compile $ setupNetwork sources

  SDL.enableKeyRepeat 60 60

  actuate network
  eventLoop frameRate sources


eventLoop :: Int -> ((AddHandler SDL.Event, Handler SDL.Event), (AddHandler Word32, Handler Word32)) -> IO ()
eventLoop rate (esdlkey, esdltick) = do
  events <- collectEvents
  startTick <- SDL.getTicks

  case events of
    Nothing -> return ()
    Just e  -> do
      (snd esdlkey) e
      (snd esdltick) startTick

  endTick <- SDL.getTicks
  
  let deltaTick = fromIntegral (endTick - startTick)
      framerate = fromIntegral (1000 `div` rate)
      
  when (deltaTick < framerate) $
    SDL.delay (framerate - deltaTick)

  --putStrLn (show events)
  eventLoop rate (esdlkey, esdltick)
  
  where
    collectEvents :: IO (Maybe SDL.Event)
    collectEvents = do
      e <- SDL.pollEvent
      case e of
        SDL.Quit    -> return Nothing
        otherwise   -> return $ Just e


setupNetwork :: forall t. Frameworks t =>
                ((AddHandler SDL.Event, Handler SDL.Event),
                 (AddHandler Word32, Handler Word32)) -> Moment t ()
setupNetwork (esdlkey, esdltick) = do
  ekey  <- fromAddHandler (fst esdlkey)
  etick <- fromAddHandler (fst esdltick)

  let

    iship = Ship {
        position = ((screenWidth `div` 2), (screenHeight `div` 2))
      , size = (5, 5)
      }

    bship = accumB iship (flip move <$> emoveship)
    
    bgame = GameState <$> bship
    
    emoveship = withDirection <$> filterE isMove ekey
      where
        isMove :: SDL.Event -> Bool
        isMove (SDL.KeyDown k) = elem (SDL.symKey k) [SDL.SDLK_LEFT, SDL.SDLK_RIGHT, SDL.SDLK_UP, SDL.SDLK_DOWN]
        isMove _               = False

        withDirection :: SDL.Event -> Direction
        withDirection (SDL.KeyDown k) = case (SDL.symKey k) of
          SDL.SDLK_LEFT  -> ToLeft
          SDL.SDLK_RIGHT -> ToRight
          SDL.SDLK_UP    -> ToUp
          SDL.SDLK_DOWN  -> ToDown

    eframe = bgame <@ etick
  
  reactimate $
    fmap render eframe
  
  return ()
