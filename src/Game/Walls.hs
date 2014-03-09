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
  } deriving (Show)

mkWalls :: Config -> Walls
mkWalls Config{cWorldSize} = Walls {
    wSpeed = 1000
  , wWalls = R.newInit (0, 0) (snd cWorldSize)
  } 

instance Movable Walls where
  move w d = w {
      wWalls = R.push (wWalls w) (1, 1)
      , wSpeed = (wSpeed w) + 500
    }

instance Renderable Walls where
  render Walls{wWalls,wSpeed} Config{cCellSize,cWorldSize} = do
    screen <- SDL.getVideoSurface

    --putStrLn (show wSpeed)
    
    loop screen 0 wWalls cCellSize
    where
      loop s i w c = do
        if i < R.length w
          then
            do
              let (l, r) = w R.! i
              (SDL.mapRGB . SDL.surfaceGetPixelFormat) s 0 50 50 >>=
                SDL.fillRect s (Just $ SDL.Rect 0 (i * cCellSize) (l * cCellSize) cCellSize)
              loop s (i + 1) w c
          else
            do return ()
