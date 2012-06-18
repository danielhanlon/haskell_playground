runLengthEncode [] = []
runLengthEncode (x:xs) = nextGroup x 1 xs
  where
    nextGroup e n [] = [(e,n)]
    nextGroup e n (y:ys)
      | e == y          = nextGroup e (n+1) ys
      | otherwise       = (e,n) : nextGroup y 1 ys 
