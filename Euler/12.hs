upTo n (x:xs)
  | x > n = []
  | otherwise = x:(upTo n xs)

triangular = [foldr (+) 0 [1..n] | n <- [1..]]
