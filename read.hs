module Main where

main = do
	--putStrLn "Please enter a Double:"
	--inpStr <- getLine
	--let inpDouble = (read inpStr)::Double
	--putStrLn ("Twice "++show inpDouble++" is "++show (inpDouble*2))

	s="Hello World!"
	x=do c <- s
			return (toUpper c)
	putStrLn x