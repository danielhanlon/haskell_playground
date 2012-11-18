import Data.Char
import System.IO

my_sequence :: [IO a] -> IO ()
my_sequence [] = return ()
my_sequence (x:xs) = do x
                        my_sequence xs

upperFile :: String -> IO ()
upperFile s = do
                  file <- readFile s
                  let ls = lines file
                  let actions = map putStrLn ( map (\l -> map toUpper l) ls)
                  my_sequence actions

my_readfilei :: FilePath -> IO (Integer -> IO Char)
my_readfilei name = do h <- openFile name ReadMode
                       let readi i = do hSeek h AbsoluteSeek i
                                        hGetChar h
                       return readi
