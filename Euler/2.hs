fibonacci :: (Ord a, Num a) a -> a§
fibonacci = [fib n | n<-[1..]]
  where
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)
