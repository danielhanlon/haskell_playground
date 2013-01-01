import Data.Array

printLines :: Show a => [[a]] -> IO()
printLines ss = (mapM_ . mapM_) (putStrLn . show) ss
  
readInt:: String -> Int
readInt = read 

main = do
  input <- readFile "number_square"
  -- Grid: 0,0 -> 19,19
  let a = listArray ((0,0),(19,19)) $ map readInt $ words input
  let products = (downProducts a) ++ (rightProducts a) ++ (downRightProducts a) ++ (upRightProducts a)
  putStrLn "Max four in a row="
  putStrLn $ show (maximum products)

--D: 0,0 -> 19,16
downProducts :: Array (Integer, Integer) Int -> [Int]
downProducts a = 
  [a!i1 * a!i2 * a!i3 * a!i4 | x <- [0..19], y <- [0..16]
                             , let i1=(y,x)
                             , let i2=(y+1,x)
                             , let i3=(y+2,x)
                             , let i4=(y+3,x)
                             ]

--R: 0,0 -> 16,19
rightProducts :: Array (Integer, Integer) Int -> [Int]
rightProducts a = 
  [a!i1 * a!i2 * a!i3 * a!i4 | x <- [0..16], y <- [0..19]
                             , let i1=(y,x)
                             , let i2=(y,x+1)
                             , let i3=(y,x+2)
                             , let i4=(y,x+3)
                             ]

--DR: 0,0 -> 16,16
downRightProducts :: Array (Integer, Integer) Int -> [Int]
downRightProducts a = 
  [a!i1 * a!i2 * a!i3 * a!i4 | x <- [0..16], y <- [0..16]
                             , let i1=(y,x)
                             , let i2=(y+1,x+1)
                             , let i3=(y+2,x+2)
                             , let i4=(y+3,x+3)
                             ]

--UR: 0,3 -> 16,19
upRightProducts :: Array (Integer, Integer) Int -> [Int]
upRightProducts a =
  [a!i1 * a!i2 * a!i3 * a!i4 | x <- [0..16], y <- [3..19]
                             , let i1=(y,x)
                             , let i2=(y-1,x+1)
                             , let i3=(y-2,x+2)
                             , let i4=(y-3,x+3)
                             ] 


