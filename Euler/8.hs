import Data.Array

fibonacci :: (Enum a, Num a) => [a] 
fibonacci = [fib n | n<-[1..]]
  where
  fib 1 = 1
  fib 2 = 1
  fib n = fib (n-1) + fib (n-2)

even_fib :: [Integer]
even_fib = filter even fibonacci

upTo n (x:xs)
  | x >= n = []
  | otherwise= x:(upTo n xs)

upToIncluding n xs = upTo (n+1) xs

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = memoized_fib (n-2) + memoized_fib (n-1)

mem_fibonnaci = [memoized_fib n | n<-[1..]]

--Euler 1
--sum $ upTo 999 [ x | x<-[1..], (x`mod`3==0 || x`mod`5==0) ]

memoizedIsPrime :: Int -> Bool
memoizedIsPrime = (map isPrime [0..] !!)
  where
    isPrime :: Int -> Bool
    isPrime n = isNotDivisibleBy 2
      where
         root_n = floor $ sqrt $ fromIntegral n
         isNotDivisibleBy f 
            | f > root_n = True
            | n `mod` f == 0 = False
            | otherwise = isNotDivisibleBy (f+1)

primes = [n | n<-[2..], memoizedIsPrime n]

primeFactors :: Int -> [Int]
primeFactors n = addPrimeFactor candidates []
  where
    candidates = upToIncluding (floor $ sqrt $ fromIntegral n) primes
    addPrimeFactor [] fs = fs
    addPrimeFactor (c:cs) fs
      | (n `mod` c == 0) = addPrimeFactor cs (c:fs)
      | otherwise = addPrimeFactor cs fs
--    addPrimeFactor c fs
--      | c > quot n 2 = fs
--      | (n `mod` c == 0) && memoizedIsPrime c = addPrimeFactor (c+1) (c:fs)
--      | otherwise = addPrimeFactor (c+1) fs

readAnInt :: IO Int
readAnInt = readLn

isPalindrome :: Int -> Bool
isPalindrome n = isPalindrome' $ show n
  where
    isPalindrome' [] = True
    isPalindrome' (x:[]) = True
    isPalindrome' (x:xs)
      | x == (head sx) = isPalindrome' $ tail sx
      | otherwise = False
        where sx = reverse xs
    
--maximum $ filter isPalindrome [ x*y | x<-[100..999], y<-[100.999] ]

isDivisibleBy :: Int -> [Int] -> Bool
isDivisibleBy n (f:[]) = ( n `mod` f == 0 )
isDivisibleBy n (f:fs) = ( n `mod` f == 0 )  && isDivisibleBy n fs

--head [n | n<-[1..], isDivisibleBy n [2..20]]
--(10 mins....)

main = do
  n <- getContents
  putStrLn $ show $ maximum $ prodFiveConsec n

prodFiveConsec :: String -> [Int]
prodFiveConsec s = pfc' s []
  where
    prod :: Char -> Char -> Char -> Char -> Char -> Int
    prod a b c d e = (read [a])*(read [b])*(read [c])*(read [d])*(read [e])
    pfc' :: String -> [Int] -> [Int]
    pfc' (a:b:c:d:e:[]) res = (prod a b c d e):res
    pfc' (a:b:c:d:e:f) res = pfc' (b:c:d:e:f) ((prod a b c d e):res)

--problem 9
--
--[a*b*c|a<-[1..1000],b<-[1..1000],c<-[1..1000],(a*a+b*b==c*c),a+b+c==1000]

f x = case x of
  0 -> 18
  1 -> 17
  2 -> 16

squares = array (1,100) [(i,i*i)|i<-[1..100]]
