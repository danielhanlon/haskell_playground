data MyResult = MyResult (Int,Int)

instance Show MyResult where
    show MyResult (x,y) = (show x) ++ "," ++ (show y)

getTuples = do 
    x <- [1..10]
    y <- [2..11]
    return $ MyResult (x,y)

main = do
    mapM putStrLn . show getTuples
