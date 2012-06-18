import Data.Char

upcaseFirst :: String -> String
upcaseFirst (c:cs) = toUpper c : cs

camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
