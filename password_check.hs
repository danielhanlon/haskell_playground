check :: String -> Bool
check pw = undefined

main = do 
  pw <- getLine
  case check pw of
    True   -> putStrLn "Good"
    False  -> putStrLn "Bad"
  
