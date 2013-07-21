
check :: String -> Bool
check pw = undefined
  where fifteen_chars = 

main = do 
  pw <- getLine
  case check pw of
    True   -> putStrLn "Good"
    False  -> putStrLn "Bad"
  
