module Main where

import Control.Monad
import Hapstack.Server (nullConf, simpleHTTP, ok, dir)

main :: IO()
main = simpleHTTP nullConf $ msum [ mzero,
									, ok "Hello, World!"
									, ok "Unreachable ServerPartT"
									]