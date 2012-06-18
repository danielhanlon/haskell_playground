myChars :: IO (Char,Char)
myChars = do  x <- getChar
              getChar
              y <- getChar
              return (x,y)


myLine :: IO String
myLine = do x <- getChar
            if x == '\n' then
              return []
            else
              do xs <- getLine
                 return (x:xs)

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do putChar x
                     myPutStr xs

--Fold version of above...
putStrF :: String -> IO()
putStrF s = foldr (\x p -> (putChar x >>= (\_ -> p))) (return ()) s
temp = \x p -> (putChar x >>= (\_ -> p))

myPutStrLn :: String -> IO ()
myPutStrLn xs = do putStr xs
                   putChar '\n'

myStrLen :: IO ()
myStrLen = do putStr "Enter a string: "
              xs <- getLine
              putStr "The string has "
              putStr (show(length xs))
              putStr " characters.\n"

