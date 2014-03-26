{-# LANGUAGE TypeFamilies #-}

module Game.Walls
       (
         Walls (..)
       , mkWalls
       ) where

import qualified Data.Vector as V
import qualified Data.RingBuffer.Class as R
import qualified Graphics.UI.SDL as SDL

import Data.Word (Word32)
import System.Random
import System.IO.Unsafe

import Game.Basics

type instance R.El (V.Vector el) = el

instance R.Initializable (V.Vector el) where
  newInit = flip V.replicate

instance R.RingBuffer (V.Vector el) where
  length      = V.length
  (!)         = (V.!)
  push vec el = V.cons el $ V.slice 0 ((V.length vec) - 1) vec


data Walls = Walls {
    wSpeed :: Word32
  , wWalls :: V.Vector (Int, Int)
  , wWidth :: Int
  } deriving (Show)

mkWalls :: Config -> Walls
mkWalls Config{cWorldSize} = Walls {
    wSpeed = 1000
  , wWalls = R.newInit (4, 4) (snd cWorldSize)
  , wWidth = fst cWorldSize
  } 

instance Movable Walls where
  move w d = let p = (wWalls w) R.! 0
                 range' = range $ ((wWidth w) `div` 2) - 4
                 l = unsafePerformIO (randomRIO $ range' (fst p))
                 r = unsafePerformIO (randomRIO $ range' (snd p))
             in
              w {
                wWalls = R.push (wWalls w) (l, r)
                }
    where
      range :: Int -> Int -> (Int, Int)
      range r p = let lo = p - 1
                      hi = p + 1
                  in
                   (if lo < 0 then 0 else lo, if hi > r then r else hi)

instance Renderable Walls where
  render Walls{wWalls,wSpeed} Config{cCellSize,cWorldSize} = do
    screen <- SDL.getVideoSurface
    
    loop screen 0 wWalls cCellSize
    where
      loop s i w c = do
        let fill = (SDL.mapRGB . SDL.surfaceGetPixelFormat) s 0 50 50
        if i < R.length w
          then
            do
              let (l, r) = w R.! i
              fill >>=
                SDL.fillRect s (Just $ SDL.Rect 0 (i * cCellSize) (l * cCellSize) cCellSize)
              fill >>=
                SDL.fillRect s (Just $ SDL.Rect (((fst cWorldSize) - r) * cCellSize) (i * cCellSize) (r * cCellSize) cCellSize)
              loop s (i + 1) w c
          else
            do return ()
