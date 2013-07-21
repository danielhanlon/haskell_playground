check :: String -> Bool
check pw = undefined

main = do 
  pw <- getLn
  case check pw of
    True   -> putStrLn "Good"
    False  -> putStrLn "Bad"
  
