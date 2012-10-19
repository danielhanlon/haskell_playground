--hh3.hs
module Main where

import Text.ParserCombinators.Parsec 

parseInput =
	do	dirs <- many dirAndSize
		eof :: Parser ()
		return dirs

data Dir = Dir Int String deriving Show

dirAndSize =
	do	size <- many1 digit
		spaces
		dir_name <- anyChar `manyTill` newline
		return (Dir (read size) dir_name)

main :: IO ()
main = do
	putStrLn "Loaded...\n"
