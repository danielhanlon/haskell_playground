fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

fibonacci :: (Enum a, Num a) => [a] 
fibonacci = [fib n | n<-[1..]]
  where
  fib 1 = 1
  fib 2 = 2
  fib n = fib (n-1) + fib (n-2)

even_fib :: [Integer]
even_fib = filter even fibonacci

upTo n (x:xs)
  | x > n = []
  | otherwise= x:(upTo n xs)

data FibFrame = FibFrame Int Int 
                deriving show
nextFibFrame :: FibFrame -> FibFrame
nextFibFrame ( FibFrame a b ) = FibFrame b (a+b)

