module Main where

import Control.Monad (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)

main :: IO ()
main = simpleHTTP nullConf $ msum [ 
            dir "hello" $ path $ \s -> ok $ "------\nHello, " ++ s ++ "\n"
                                  ]

