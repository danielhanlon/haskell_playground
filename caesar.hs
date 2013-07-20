import Data.Char (ord, chr)
import System.Console.ReadLine (readline)

main = do
  maybeLine <- readline "% "
  case maybeLine of
    Nothing       -> return ()
    Just "exit"   -> return ()
    Just line     -> putStrLn $ caesar 3 line

caesar :: Int -> String -> String
caesar shift plaintext = caesar' plaintext
  where
    enc c = [chr newC_chr]
      where c_chr = ord c
            newC_chr = shift + c_chr `mod` 26
    caesar' (c:cs) = enc c ++ caesar' cs
    caesar' (c:[]) = enc c
