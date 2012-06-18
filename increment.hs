import System.IO
import Data.Char 

getCh :: IO Char
getCh  = do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

incrementGame :: IO ()
incrementGame = getCh >>= \x -> putChar ( getIncChar x ) >>= \y -> putChar '\n'

getIncChar :: Char -> Char
getIncChar c = intToDigit d
  where
    d = number + 1
    number = digitToInt c


--type declarations
--type String = [Char]
--
--type Pos = (Int,Int)
--origin :: pos
--origin = (0,0)
--left :: Post -> Pos
--
--type Pair a = (a,a)
--mult :: Pair Int -> Int
--mult (x,y) = x*y
--left (x,y) = (x-1,y)
