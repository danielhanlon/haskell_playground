data MyResult = MyResult (Int,Int)

instance Show MyResult where
    show (MyResult (x,y)) = (show x) ++ "," ++ (show y)

getTuples = do 
    x <- [1..5]
    let y = 2
    return $ MyResult (x,y)

main = mapM putStrLn $ mapM show getTuples
