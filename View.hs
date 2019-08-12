module View where

import Model
import System.Console.ANSI

printBlockGraph :: (Integral a, Integral b, Integral c)
  => Graph a b c -> IO () 
printBlockGraph g = do 
  let vs = [[greyTransform v | v <- row] | row <- values g]
  let p = \row -> putStrLn $ concatMap show row
  mapM_ p $ reverse [row | row <- vs]


printGraph :: (Show a, Show b, Show c) 
  => Graph a b c -> Int -> IO ()
printGraph graph width = do
  let rows = zip (yAxis graph) (values graph)
  let print = \row -> printRowWithLabel row width
  mapM_ print (reverse rows)
  printXAxis (xAxis graph) width

printRowWithLabel :: (Show a, Show b) 
  => (a, [b]) -> Int -> IO ()
printRowWithLabel row width = do
  setSGR [ SetColor Foreground Vivid Red ]
  putStr $ (\x -> yPad (shorten x width)) (fst row) width
  setSGR []
  printList $ map (\x -> viewValue x width) (snd row)
   
printXAxis :: (Show a) => [a] -> Int -> IO ()
printXAxis axis width = do 
  setSGR [ SetColor Foreground Vivid Red ]
  putStr $ rightPad "" width
  printList $ map (\x -> viewValue x width) axis
  setSGR []

printList :: [String] -> IO ()
printList list = putStrLn $ concat list

viewValue :: (Show a) => a -> Int -> String 
viewValue value width = rightPad (shorten value width) width

shorten :: (Show a) => a -> Int -> String
shorten value width = take (pred width) $ show value

rightPad :: String -> Int -> String
rightPad string width = string ++ spaces
  where len = length string
        spaces = if len > width
                 then ""
                 else take (width - len) $ repeat ' '

yPad :: String -> Int -> String
yPad s width = spaces ++ string
  where len = length string
        string = s ++ " "
        spaces = if len > width
                 then ""
                 else take (width - len) $ repeat ' '

printRainbow :: String -> IO ()
printRainbow s = putStrLn "Testing"  
