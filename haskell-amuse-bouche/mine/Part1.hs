import Data.List
main = readFile "poem" >>= putStr . unlines . sort . lines
