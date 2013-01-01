import Data.Array
import Control.Monad (guard)

data Rank = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Show, Read, Enum, Ix)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Ord, Show, Read, Enum, Ix)

data GenCard = GenCard Rank Suit
               deriving (Eq, Ord, Show, Read)
 
genCardToInt :: GenCard -> Int
genCardToInt (GenCard r s) = lookup ! (r,s)
  where
    lookup = listArray ((Two,Clubs),(Ace,Spades)) [x|x<-[0..51]]

fibs :: Int -> Array Int Int
fibs n = a
  where a = array (0,n) ([(0,1), (1,1)]
              ++ [(i, a!(i-2) + a!(i-1)) | i<-[2..n]])

wavefront :: Int -> Array (Int,Int) Int
wavefront n = a where
              a = array ((1,1),(n,n))
                    ([((1,j),1) | j <- [1..n]] ++
                     [((i,1),1) | i <- [2..n]] ++
                     [((i,j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                                | i <- [2..n], j <- [2..n]])

printLines :: Show a => [[a]] -> IO()
printLines ss = (mapM_ . mapM_) (putStrLn . show) ss
  
readInt :: String -> Int
readInt = read 

testInput = "12 13 14 15\n16 17 18 19\n20 21 22 23\n24 25 26 27"

main = do
  input <- getContents
  --let list2D = map (map readInt . words) $ lines input
  --let list = map readInt $ words input
  --printLines list2D
  let la = listArray ((0,0),(19,19)) $ map readInt $ words input
  putStrLn (show $ rightProducts la)

--Product of all fours in a row
--Diagonal grid: 0,0 -> 19,19
--R: 0,0 -> 16,19
--D: 0,0 -> 19,16
--DR: 0,0 -> 16,16
--UR: 0,3 -> 16,19

rightProducts :: Array (Integer, Integer) Int -> [Int]
rightProducts a = 
  [(a!i1 * a!i2 * a!i3 * a!i4) | x <- [0..], y <- [0..]
                               , let i1=(x,y)
                               , let i2=(x+1,y)
                               , let i3=(x+2,y)
                               , let i4=(x+3,y) , i4 <= bounds a
                               ]
