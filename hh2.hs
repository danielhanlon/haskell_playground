module Main where

import System.Console.Readline

--stripCR :: [Char] -> [Char]
--stripCR [] = []
--stripCR (a:as) = a:(stripCR as) , a='\n'
--                 stripCR as , otherwise

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  maybeLine <- readline "% "
  case maybeLine of 
    Nothing     -> return () -- EOF / control-d
    Just "exit" -> return ()
    Just line -> do addHistory line
                    putStrLn $ "The user input: " ++ (show line)
                    readEvalPrintLoop

main :: IO ()
main =
  do
    putStrLn "Please enter your name:"
    x <- getLine
    let greeting = "Hello " ++ x
    putStrLn greeting

    let colourQuestion = "What's your favourite colour " ++
                         (filter (\c->c/='\n')  x) ++ "?\n"
    putStrLn colourQuestion

    colour <- getLine
    let colourAnswer = colour ++ " is a nice colour!"
    putStrLn colourAnswer

    readEvalPrintLoop
