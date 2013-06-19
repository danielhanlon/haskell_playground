{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

pCircle1' = circle1 # fc blue # lw 0.05 # lc purple # dashing [0.2,0.05] 0
main = defaultMain $ square 1 # fc aqua `atop` pCircle1'
