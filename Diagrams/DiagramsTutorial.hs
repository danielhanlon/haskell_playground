{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

circle1 = circle 1 # fc blue
                   # lw 0.05
                   # lc purple
                   # dashing [0.2, 0.05] 0

main = defaultMain circle1
