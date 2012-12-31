import Data.Array

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

main = do
  input <- getContents
  let list = map (map readInt . words) $ lines input
  let la = listArray ((1,1),(20,20)) list
  printLines list
