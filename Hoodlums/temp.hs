import Data.Char

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let l = lines file
                  putStrLn $ map $ map toUpper l
