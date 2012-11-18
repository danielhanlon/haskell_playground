import Data.Char

sequence_ :: [IO a] -> IO ()
sequence_ [] = return ()
sequence_ (x:xs) = do x
                      sequence_ xs

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let ls = lines file
                  sequence_ $ map  (\l -> map toUpper l) ls
