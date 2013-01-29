module Main where

import Control.Monad
import Happstack.Server (nullConf, simpleHTTP, ok, dir)

main :: IO ()
main = simpleHTTP nullConf $ msum [ dir "hello" $ ok "Hello, World!"
                                  , dir "goodbye" $ ok "Goodbye, World!"
                                  ]
