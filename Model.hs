module Model where

-- 3 dimensional graphs

import Data.Word
import Data.Bits
import System.Console.ANSI
import Data.Colour.SRGB (sRGB24)

data Graph a b c = Graph
                { xAxis :: [a]
                , yAxis :: [b] 
                , values :: [[c]]
                } deriving (Show)

generateGraph :: (Show a, Show b, Show c) 
  => [a] -> [b] -> (a -> b -> c) -> Graph a b c
generateGraph xs ys f = Graph { xAxis = xs, yAxis = ys, values = vs }
  where vs = [[f x y | x <- xs] | y <- ys]

listFromRange :: (Num a, Enum a) => a -> a -> a -> [a]
listFromRange start stop step = [start, start + step .. stop]

data Block = Block (Word8, Word8, Word8)

instance Show Block where
  show (Block (x, y, z))
    = setSGRCode [SetRGBColor Foreground $ sRGB24 x y z] 
      ++ "██"
      ++ setSGRCode [Reset]

greyTransform :: (Integral a) => a -> Block
greyTransform i = Block (c, c, c)
  where c = complement $ fromIntegral (div i 12)
