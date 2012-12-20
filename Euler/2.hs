fibonacci :: (Enum a, Num a) => [a] 
fibonacci = [fib n | n<-[1..]]
  where
  fib 1 = 1
  fib 2 = 1
  fib n = fib (n-1) + fib (n-2)

even_fib :: [Integer]
even_fib = filter even fibonacci

upTo n (x:xs)
  | x > n = []
  | otherwise= x:(upTo n xs)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = memoized_fib (n-2) + memoized_fib (n-1)

mem_fibonnaci = [memoized_fib n | n<-[1..]]

--Euler 1
--sum $ upTo 999 [ x | x<-[1..], (x`mod`3==0 || x`mod`5==0) ]

