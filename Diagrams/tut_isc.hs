{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

pCircle1' = circle 1 # fc blue # lw 0.05 # lc purple # dashing [0.2,0.05] 0
main = defaultMain $ pCircle1' # scale 0.3 ||| pCircle1' ===  square 1 # fc aqua 
