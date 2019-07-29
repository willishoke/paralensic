module Model where

data Graph a b c = Graph
                { xAxis :: [a]
                , yAxis :: [b] 
                , values :: [[c]]
                } deriving (Show)

generateGraph :: (Show a, Show b, Show c) =>
  [a] -> [b] -> (a -> b -> c) -> Graph a b c
generateGraph xs ys f = Graph { xAxis = xs, yAxis = ys, values = vs }
  where vs = [[f x y | x <- xs] | y <- ys]

listFromRange :: (Num a, Enum a) => a -> a -> a -> [a]
listFromRange start stop step = [start, start + step .. stop]
