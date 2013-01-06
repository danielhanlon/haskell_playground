-- What is the value of the first triangle number
-- to have over five hundred divisors?

import System.Exit
import System.Environment
import System.IO
import qualified Data.List as L

upTo n (x:xs)
  | x > n = []
  | otherwise = x:(upTo n xs)

upToIncluding n xs = upTo (n+1) xs

triangular :: [Int]
triangular = [foldr (+) 0 [1..n] | n <- [1..]]

tri = [floor (n*(n+1)/2)|n<-[1..]]

numberOfFactors n = foldr ((+) . isFactor) 0 [1..n] 
	where
		isFactor f 
			| n `mod` f == 0	= 1
			| otherwise 		= 0

memoizedIsPrime :: Int -> Bool
memoizedIsPrime = (map isPrime [0..] !!)
  where
    isPrime :: Int -> Bool
    isPrime n = isNotDivisibleBy 2
      where
         root_n = floor $ sqrt $ fromIntegral n
         isNotDivisibleBy f 
            | f > root_n = True
            | n `mod` f == 0 = False
            | otherwise = isNotDivisibleBy (f+1)

primes = [n | n<-[1..], memoizedIsPrime n]

facs :: Int -> Int
facs n 
	| n == 1 = 1
	| n > 1 = 2 * fac' n 1 1
	where
		fac' :: Int -> Int -> Int -> Int
		fac' n factors primeCount 
			| ( primes!!primeCount * primes!!primeCount > n ) = factors 
			| otherwise =
				fac' n (factors * (power+1)) (primeCount+1)
					where
						cp = primes!!primeCount
						power = fac'' 0 cp n
						fac'' pow pri nn
							|	( nn `mod` pri == 0 ) = fac'' (pow+1) (pri*cp) (nn `div` pri)
							|	otherwise = pow


