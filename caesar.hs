import Data.Char (ord, chr)

main = do
--  input <- getChar
  putChar $ caesar 3 "Hello World!"

caesar :: Int -> String -> String
caesar shift plaintext = caesar' plaintext
  where
    enc c = [chr newC_chr]
      where c_chr = ord c
            newC_chr = shift + c_chr `mod` 26
    caesar' (c:cs) = enc c ++ caesar' cs
    caesar' (c:[]) = enc c
