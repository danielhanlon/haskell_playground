module Main where

main =
  do putStrLn "Do you like Haskell? [yes/no]"
     answer <- getLine
     case answer of
          "yes" -> putStrLn "Well done!"
--          "yes" -> putStrLn "something else"
          "no"  -> putStrLn "Think again..."
--          _     -> putStrLn "What?"
