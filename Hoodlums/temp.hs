import Data.Char

sequence_ :: [IO a] -> IO ()
my_sequence [] = return ()
my_sequence (x:xs) = do x
                        my_sequence xs

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let ls = lines file
                  my_sequence $ map  (\l -> map toUpper l) ls
