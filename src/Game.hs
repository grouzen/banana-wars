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
      conf       = mkConfig s w
  
  screen  <- SDL.setVideoMode sw sh 32 [SDL.SWSurface]
  sources <- (,) <$> newAddHandler <*> newAddHandler
  network <- compile $ setupNetwork conf sources

  SDL.setCaption "Banana Wars" ""
  SDL.enableKeyRepeat 50 50

  actuate network
  eventLoop 50 sources


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
        SDL.Quit    -> return Nothing
        otherwise   -> return $ Just e


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

    bship :: Behavior t Ship
    bship = accumB (gShip igame) (flip move <$> emoveship)

    bwalls :: Behavior t Walls
    bwalls = accumB (gWalls igame) (flip move <$> emovewalls)

    bgame :: Behavior t GameState
    bgame = GameState <$> bship <*> bwalls

    emovewalls :: Event t Direction
    emovewalls = ToDown <$ eperiod (wSpeed <$> bwalls)

    eperiod :: Behavior t Word32 -> Event t (Word32, Word32)
    eperiod b = filterE (\(t, n) -> rem t n > 0) $ accumE (0, 0) (f <$> b <@> etick)
      where
        f :: Word32 -> Word32 -> (Word32, Word32) -> (Word32, Word32)
        f n c (p,_) | c - p > n = if rem c n == 0 then (c + 1, n) else (c, n)
                    | otherwise = ((p `div` n) * n, n)

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

  -- Debug
  reactimate $
    flip fmap (eperiod (wSpeed <$> bwalls)) $ \w -> do
      putStrLn $ show (fst w) ++ " @ " ++ show (snd w)
