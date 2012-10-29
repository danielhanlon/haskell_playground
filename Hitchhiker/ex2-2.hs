main = do
  putStrLn "Greetings! What's your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, "++inpStr++"!"
  putStrLn "What is your favourite colour?"
  inputStr2 <- getLine
  putStrLn $ "Your favourite colour is "++inputStr2
