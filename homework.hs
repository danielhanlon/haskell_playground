drop :: [Int] -> [Int]
drop (x:xs) = [n|n<-xs, n `mod` 23==7] 
first51 = take 51 (Main.drop [1..])
startNumber = last first51
homework :: [Int]
homework = take 500 (Main.drop [startNumber..])
