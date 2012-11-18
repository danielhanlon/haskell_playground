import Data.Char

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let ls = lines file
                  let actions = map  (\l -> map toUpper l) ls
                  do head actions
                     actions !! 1
                     last actions
