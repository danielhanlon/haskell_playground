import Data.Char (ord, chr, toUpper)

main = do
  line <- getLine
  if isPal line
    then putStrLn "Palindrome!"
    else putStrLn "..not a palindrome"

isPal :: Eq a => [a] -> Bool
isPal []     = True
isPal (a:[]) = True
isPal (a:as) = a == s && isPal rest
  where
    sa = reverse as
    s = head sa
    rest = tail sa
  
convert :: (Double, [Char]) -> (Double, [Char])
convert d u
  | u == "m"    = ( d*m2yd, "yd"  )
  | u == "yd"   = ( d/m2yd, "m"   )
  | u == "L"    = ( d*l2gal, "gal")
  | u == "gal"  = ( d/l2gal, "L"  )
  | u == "kg"   = ( d*kg2lb, "lb" )
  | u == "lb"   = ( d/kg2lb, "kg" )
    where
      m2yd  = 1.09361
      l2gal = 0.264172
      kg2lb = 2.20462
