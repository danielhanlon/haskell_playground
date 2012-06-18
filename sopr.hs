import System.IO

scaling prices power = do
  myPrint result
    where
      result = zip prices (map sopr prices)
      sopr p = 100*((minimum prices)/p)**power
      myPrint :: (Show a, RealFrac a1) => [(a, a1)] -> IO ()
      myPrint [] = putStrLn (replicate 20 '-')
      myPrint ((r1,r2):rs) =
        do 
           putStrLn s
           myPrint rs
             where s = "( £"++show r1++"/TB -> "++show (round r2)++"% )"

