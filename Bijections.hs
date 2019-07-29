module Bijections where

pair :: (Int, Int) -> Int
pair (x, y) = z
  where z = (+y) . (`div` 2) $ (*) (x+y-2) (x+y-1)

-- will do some weird things given negative values!
depair :: Int -> (Int, Int)
depair z = (x, y)
  where x = (succ w) - (pred y)
        y = z - t
        t = (`div` 2) . (* w) $ succ w
        w = floor . (/2) $ (sqrt i) - 1
        i = fromIntegral . succ $ (pred z) * 8
{--

Partitioning scheme

     |  I 
 II  |______
_____|0̲|
       |  IV
  III  |
  
--}

pairQuadrants :: (Int, Int) -> Int
pairQuadrants (0, 0) = 0
pairQuadrants (x, y) = if q1 then 0 else 0 
  where q1 = signum x == 1 && signum y == 1
        q2 = signum x == (-1) && signum y == 1
        q3 = signum x == (-1) && signum y == (-1)
        q4 = signum x == 1 && signum y == (-1)
 
depairQuadrants :: Int -> (Int, Int)
depairQuadrants 0 = (0, 0)
depairQuadrants z = if m == 0 then depair m
                    else if m == 1 then depair m
                    else if m == 2 then depair m
                    else if m == 3 then depair m
                    else (0, 0)
  where m = mod z 4

natToInt:: Int -> Int
natToInt i = if even i
             then negate $ div i 2
             else div (succ i) 2

intToNat :: Int -> Int
intToNat n = if signum n < 1
             then ((*2) . negate) n
             else (pred . (*2)) n

intToQuadInt :: Int -> Int
intToQuadInt i = 0 
