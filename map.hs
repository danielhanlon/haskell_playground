piGuess :: Int -> Double
piGuess n = sum (map f [1..n])

f :: Int -> Double
f x = 4*(-1)^(x+1) / (2.0*k -1)
  where k = fromIntegral x
