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

--TODO 
--loop :: State Model -> IO ()
--add stateful computation to loop

loop :: IO ()
loop = do 
  terminalSize <- getTerminalSize 
  let t = unwrap terminalSize
  let width = 5
  let xSize = div (pred $ snd t) (2 * width)
  let ySize = div (fst t) 2
  let xAxis = listFromRange (succ $ negate xSize) (pred xSize) 1
  let yAxis = listFromRange (succ $ negate ySize) (pred ySize) 1 
  let f = \x -> \y -> natToInt $ szudzikPair (intToNat x, intToNat y) 
  let g = generateGraph xAxis yAxis f
  printGraph g width

unwrap :: Maybe (Int, Int) -> (Int, Int)
unwrap m = case m of Nothing -> (0, 0)
                     Just (x, y) -> (x, y)
          
