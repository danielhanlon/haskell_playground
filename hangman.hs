--Hangman game
--(learning the IO Monad...)

import System.IO 

hangman :: IO ()
hangman = 
  do putStrLn "Think of a word: "
     word <- sgetLine
     putStrLn "Try to guess it:"
     guess word

getCh :: IO Char
getCh  = do 
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

sgetLine :: IO String
sgetLine =
  do x <- getCh
     if x == '\n' then
         do putChar x
            return []
       else
         do putChar '-'
            xs <- sgetLine
            return (x:xs)

guess :: String -> IO ()
guess word = do 
              putStr "> "
              gw <- getLine
              if gw == word then
                  putStrLn "Correct!"
                else
                  do putStrLn (diff word gw)
                     guess word

diff :: String -> String -> String
--diff [] []  = []
--diff (w:ws) (x:xs) | w==x =  w:diff ws xs 
--                   | otherwise = '-':diff ws xs
--diff _ _ = []
diff xs ys = [if elem x ys then x else '-' | x <- xs]
