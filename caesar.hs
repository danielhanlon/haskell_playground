import Data.Char (ord, chr, toUpper)

main = do
  line <- getLine
  putStrLn $ caesar 3 line

caesar :: Int -> String -> String
caesar shift plaintext = caesar' plaintext
  where
    enc c = [chr newC_chr]
      where c_chr = (ord $ toUpper c)-65
            newC_chr = 65 + ((shift + c_chr) `mod` 26)
    caesar' (c:[]) = enc c
    caesar' (c:cs) = enc c ++ caesar' cs
