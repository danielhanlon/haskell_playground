data FibFrame = FibFrame Ord Ord 
instance Show FibFrame where
  show (FibFrame a b) = show b
instance Eq FibFrame where
  (FibFrame a1 b1) == (FibFrame a2 b2) = ( a1==b1 && a2==b2)
instance Ord FibFrame where
  (FibFrame a1 b1) <= (FibFrame a2 b2) = b1<=b2
nextFibFrame :: FibFrame -> FibFrame
nextFibFrame ( FibFrame a b ) = FibFrame b (a+b)
fib = iterate nextFibFrame (FibFrame 1 1)
