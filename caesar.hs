import Data.Char (ord, chr)

main = do
  putChar caesar getChar

caesar :: Int -> String -> String
caesar shift plaintext = caesar' plaintext
  where
    caesar' (c:cs) = [newC] ++ caesar' cs
    caesar' (c:[]) = [newC]
    c_chr = ord c
    newC_chr = shift + c_chr `mod` 26
    newC = chr newC_chr
