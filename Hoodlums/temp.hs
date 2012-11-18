import Data.Char

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let ls = lines file
                  do 
                    map putStrLn ( map  (\l -> map toUpper l) ls)
