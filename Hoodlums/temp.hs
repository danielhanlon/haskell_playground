import Data.Char

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  map putStrLn $ lines file
