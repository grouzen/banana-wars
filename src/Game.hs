{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Graphics.UI.SDL as SDL
import Data.Word (Word32)
import Control.Monad

import Game.Basics
import Game.Ship
import Game.Walls
import Game.GameState

main :: IO ()
main = do
  -- game config
  let s@(sw, sh) = (640, 480)
      w@(ww, wh) = (32, 24)
      framerate  = 50
      conf       = mkConfig s w framerate
  
  screen  <- SDL.setVideoMode sw sh 32 [SDL.SWSurface]
  sources <- (,) <$> newAddHandler <*> newAddHandler
  network <- compile $ setupNetwork conf sources

  SDL.setCaption "Banana Wars" ""
  SDL.enableKeyRepeat framerate framerate

  actuate network
  eventLoop framerate sources

eventLoop ::
  Int ->
  ((AddHandler SDL.Event, Handler SDL.Event),
   (AddHandler Word32, Handler Word32)) -> IO ()
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

  eventLoop rate (esdlkey, esdltick)
  
  where
    collectEvents :: IO (Maybe SDL.Event)
    collectEvents = do
      e <- SDL.pollEvent
      case e of
        SDL.Quit  -> return Nothing
        otherwise -> return $ Just e

setupNetwork :: forall t. Frameworks t =>
                Config ->
                ((AddHandler SDL.Event, Handler SDL.Event),
                 (AddHandler Word32, Handler Word32)) -> Moment t ()
setupNetwork conf (esdlkey, esdltick) = do
  ekey  <- fromAddHandler (fst esdlkey)
  etick <- fromAddHandler (fst esdltick)

  let

    igame :: GameState
    igame = mkGameState conf
      
    bgame :: Behavior t GameState
    bgame = GameState <$> bship <*> bwalls

    bship :: Behavior t Ship
    bship = accumB (gShip igame) (flip move <$> emoveship)

    bwalls :: Behavior t Walls
    bwalls = accumB (gWalls igame) (flip move <$> emovewalls)

    -- in game ticks, e.g 10 = 50ms * 10 = 500ms, if game tick = 50ms
    -- TODO: make common interface for such needs
    bwallsfreq :: Behavior t Int
    bwallsfreq = freq <$> (wSpeed <$> bwalls)
      where
        freq :: Word32 -> Int
        freq s = fromIntegral s `div` (1000 `div` (cFrameRate conf))

    emovewalls :: Event t Direction
    emovewalls = ToDown <$ filterE (== 0) (accumE 0 $ update <$> (bwallsfreq <@ etick))
      where
        update :: Int -> Int -> Int
        update freq c = let c' = c + 1
                        in if c' `mod` freq == 0
                           then 0
                           else c'

    emoveship :: Event t Direction
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

    eframe :: Event t GameState
    eframe = bgame <@ etick
  
  reactimate $
    fmap (\g -> render g conf) eframe

