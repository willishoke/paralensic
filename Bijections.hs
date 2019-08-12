module Bijections where

-- will do some weird things given negative values!
cantorPair :: (Int, Int) -> Int
cantorPair (x, y) = z
  where z = (+y) . (`div` 2) $ (x+y) * (succ $ x+y)

cantorDepair :: Int -> (Int, Int)
cantorDepair z = (x, y)
  where x = (succ w) - (pred y)
        y = z - t
        t = (`div` 2) . (* w) $ succ w
        w = floor . (/2) $ (sqrt i) - 1
        i = fromIntegral . succ $ (pred z) * 8

szudzikPair :: (Int, Int) -> Int
szudzikPair (x, y) = if x == max x y then s1 else s2
  where s1 = (+x) . (+y) . (*x) $ x
        s2 = (+x) . (*y) $ y

szudzikDepair :: Int -> (Int, Int)
szudzikDepair z = if c1 then r1 else r2
  where c1 = z - y < x
        r1 = (z - y, x) 
        r2 = (y, z - y - x)
        x = floor $ sqrt $ fromIntegral z
        y = x * x

{--
depair :: Int -> (Int, Int)
depair z = (x, y)
  where x = (succ w) - (pred y)
        y = z - t
        t = (`div` 2) . (+w) . (*w) . succ $ w
        w = floor . (/2) . (-1) . sqrt $ i
        i = fromIntegral . succ . (*8) $ z 
--}

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
pairQuadrants (x, y) = if q1 then cantorPair (x, succ y)
                         else 0
  where q1 = signum x >= 0 && signum y >= 1
        q2 = signum x <= 0 && signum y >= 0
        q3 = signum x <= 0 && signum y <= 0
        q4 = signum x >= 0 && signum y <= 0

{-- 
depairQuadrants :: Int -> (Int, Int)
depairQuadrants 0 = (0, 0)
depairQuadrants z = if m == 0 then depair m
                    else if m == 1 then depair m
                    else if m == 2 then depair m
                    else if m == 3 then depair m
                    else (0, 0)
  where m = mod z 4
--}

natToInt :: Int -> Int
natToInt i = if even i
             then negate $ div i 2
             else div (succ i) 2

intToNat :: Int -> Int
intToNat n = if signum n < 1
             then ((*2) . negate) n
             else (pred . (*2)) n

evenToInt :: Int -> Int
evenToInt =  (`div` 2)

intToEven :: Int -> Int
intToEven = (*2)

oddToInt :: Int -> Int
oddToInt = (`div` 2) . succ

intToOdd :: Int -> Int
intToOdd = pred . (*2)

