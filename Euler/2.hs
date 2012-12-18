fibonacci :: (Ord a, Num a) => [a] 
fibonacci = [fib n | n<-[1..]]
  where
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)

even_fib :: [Integer]
even_fib = [x | x<-fibonacci, x `mod`2 ==0]

sumFibUpTo total = sumFib 1
  where
    sumFib 1 = 1 + sumFib 2
    sumFib n = 
      | fib n  < total, fib n + sumFib (n+1)
      | otherwise, 0
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)
