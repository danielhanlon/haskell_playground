fibonacci :: (Ord a, Num a) => [a] 
fibonacci = [fib n | n<-[1..]]
  where
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)

even_fib :: [Integer]
even_fib = [x | x<-fibonacci, x `mod`2 ==0]
