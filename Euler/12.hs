upTo n (x:xs)
  | x > n = []
  | otherwise = x:(upTo n xs)

upToIncluding n xs = upTo (n+1) xs

triangular :: [Int]
triangular = [foldr (+) 0 [1..n] | n <- [1..]]

numberOfFactors :: Int -> Int
numberOfFactors n = foldr ((+) . isFactor) 0 [1..n] 
	where
		isFactor f 
			| n `mod` f == 0	= 1
			| otherwise 		= 0

main = do
  args@(~( aString : aInteger : [] ) ) <- getArgs
  let parsed@( ~[(n,_)] ) = reads aInteger
  if length args /= 2 || L.null parsed
    then do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <string> <integer>"
      exitFailure
    else do
      doStuffWith aString n