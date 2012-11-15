getCombinations :: [a] -> [[a]]
getCombinations = do
    a <- [1..]
    b <- [1..] 
    c <- [1..]
    return (a,b,c)
