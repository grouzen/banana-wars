
module Game.Basics
       (
         Movable    (..)
       , Renderable (..)
       , Direction  (..)
       , Config     (..)
       , mkConfig
       ) where

class Movable a where
  move :: a -> Direction -> a

class Renderable a where
  render :: a -> Config -> IO ()
  

data Config = Config {
    cResolution :: (Int, Int)
  , cWorldSize  :: (Int, Int)
  , cCellSize   :: Int
  }

mkConfig :: (Int, Int) -> (Int, Int) -> Config
mkConfig s w = let c = (fst s) `div` (fst w)
               in
                Config {
                    cResolution = s
                  , cWorldSize  = w
                  , cCellSize   = c
                  }

data Direction = ToLeft
               | ToRight
               | ToUp
               | ToDown

