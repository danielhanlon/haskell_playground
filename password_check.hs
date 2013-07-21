main = do 
  pw <- readLn
  case check pw of
    True   -> putStrLn "Good"
    False  -> putStrLn "Bad"
  
 check :: String -> Bool
 check pw = undefined
