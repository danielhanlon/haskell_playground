getTuples = do 
    x <- [1..10]
    y <- [2..11]
    return (x,y)

main = do
    mapM putStrLn getTuples
