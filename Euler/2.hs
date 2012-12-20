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

primes = [n | n<-[1..], memoizedIsPrime n]

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

main = do
  number <- readAnInt
  putStrLn $ show $ maximum $ primeFactors number
