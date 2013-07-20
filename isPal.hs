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
  
