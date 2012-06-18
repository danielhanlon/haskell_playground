qsort :: Ord a => [a] -> [a]
qsort[] = []
qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a|a<-xs, a<=x]
    larger = [b|b<-xs, b>x]

factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial (n-1)


abs n | n>=0 = n
      | otherwise = -n


concat :: [[a]] -> [a]
concat xss = [x|xs<-xss, x<-xs]

factors :: Int -> [Int]
factors n = [x|x<-[1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes_below :: Int -> [Int]
primes_below n = [x|x<-[1..n], prime x == True]

myprod :: Num n => [n] -> n
myprod [] = 1
myprod (x:xs) = x*myprod xs

