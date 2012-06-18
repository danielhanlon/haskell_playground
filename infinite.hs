--Prime numbers
drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [n|n<-xs, n`mod`x/=0]

drop_all:: [Int] -> [Int]
drop_all (x:xs) = x:drop_all( drop_mult x xs )

primes :: [Int]
primes = drop_all[2..]

--Hemming problem
merge :: Ord a=>[a]->[a]->[a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x<y = x:merge xs (y:ys)
                    | x==y = x:merge xs ys
                    | x>y = y:merge (x:xs) ys

hemming :: [Int]
hemming = 1:merge (map (2*) hemming) ( merge (map (3*) hemming) (map (5*) hemming) )

--IO
--
echo :: IO ()
echo = getChar >>= putChar

gets::Int->IO String
gets 0 = return []
gets (n+1) = getChar >>=
            \x -> return x
