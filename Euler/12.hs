upTo n (x:xs)
  | x > n = []
  | otherwise = x:(upTo n xs)

triangular :: [Int]
triangular = [foldr (+) 0 [1..n] | n <- [1..]]

numberOfFactors :: Int -> Int
numberOfFactors n = foldr ((+) . isFactor) 0 [1..n] 
	where
		isFactor f 
			| n `mod` f == 0	= 1
			| otherwise 		= 0

upTo n (x:xs)
  | x >= n = []
  | otherwise= x:(upTo n xs)

upToIncluding n xs = upTo (n+1) xs