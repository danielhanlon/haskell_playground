main = do
  putStrLn "Please enter a double:"
  inpStr <- getLine
  let inpDouble = (read inpStr)::Double
  putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble *2))


data Color = Red | Green | Blue
instance Show Color where
  show Red  = "Red"
  show Green= "Green"
  show Blue = "Blue"

instance Read Color where
  readsPrec _ value =
    tryParse [("Red",Red), ("Green",Green), ("Blue",Blue)]
    where tryParse [] = []
          tryParse ((attempt,result):xs)=
            if (take (length attempt) value) == attempt
              then [(result,drop (length attempt) value)]
              else tryParse xs

trim :: String -> String
trim s = trimHead $ reverse $ trimHead s
  where
    trimHead [] = []
    trimHead (x:xs)
      | x == ' '  = trimHead xs
      | otherwise = x:xs
 
fibonacci :: [Int]
fibonacci = map fib [1..]
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
