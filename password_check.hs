import Data.Char (isUpper, isLower, isNumber)

check :: String -> Bool
check pw = fifteen_chars && uppercase && lowercase && number
  where
    fifteen_chars = length pw >= 15
    uppercase = any (\c -> isUpper c) pw
    lowercase = any (\c -> isLower c) pw
    number = any (\c -> isNumber c) pw
  
main = do 
  pw <- getLine
  case check pw of
    True   -> putStrLn "Good"
    False  -> putStrLn "Bad"
  
