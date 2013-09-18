data MyResult = MyResult (Int,Int) deriving Show

getTuples = do 
    x <- [1..10]
    y <- [2..11]
    return $ MyResult (x,y)

main = do
    mapM putStrLn getTuples
