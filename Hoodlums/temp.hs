import Data.Char

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let ls = lines file
                  map  (\l -> map toUpper l) ls
