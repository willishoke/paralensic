import Bijections
import View
import Model

import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do 
  initialize
  loop

initialize :: IO ()
initialize = do
  clearScreen
  setTitle "Paralensic"
  setSGR [ SetColor Foreground Vivid Red ]
  putStrLn "Paralensic" 
  hSetBuffering stdin NoBuffering
 
loop :: IO ()
loop = do 
  terminalSize <- getTerminalSize 
  let t = unwrap terminalSize
  let width = 5
  let x = [1.. (pred $ div (pred $ snd t) width)]
  let y = [1.. ((fst t) - 3)]  
  let g = generateGraph x y (\a -> \b -> (fromIntegral a) / (fromIntegral b))
  printGraph g width

unwrap :: Maybe (Int, Int) -> (Int, Int)
unwrap m = case m of Nothing -> (0, 0)
                     Just (x, y) -> (x, y)
          
