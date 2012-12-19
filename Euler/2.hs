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

data FibFrame = FibFrame Ord Ord 
instance Show FibFrame where
  show (FibFrame a b) = show b
instance Eq FibFrame where
  (FibFrame a b == FibFrame c d) = b==d
instance Ord FibFrame where
  (FibFrame a1 b1) <= (FibFrame a2 b2) = b1<=b2
  (FibFrame a1 b1) == (FibFrame a2 b2) = ( a1==b1 && a2==b2)
nextFibFrame :: FibFrame -> FibFrame
nextFibFrame ( FibFrame a b ) = FibFrame b (a+b)
fib = iterate nextFibFrame (FibFrame 1 1)
