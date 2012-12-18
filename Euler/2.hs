fibonacci :: (Enum a, Num a) => [a] 
fibonacci = [fib n | n<-[1..]]
  where
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)

even_fib :: [Integer]
even_fib = [x | x<-fibonacci, x `mod`2 ==0]

fibUpTo :: Integer -> [Integer]
fibUpTo total = sumFib (1,[])
  where
    sumFib (1,_) = sumFib (2,[1])
    sumFib (2,x) = sumFib (3,x:2)
    sumFib (n,x)  
      | n <= total = sumFib(fib n, x:sumFib (n+1))
      | otherwise = x
    fib 1 = 1
    fib 2 = 2
    fib n = fib (n-1) + fib (n-2)
