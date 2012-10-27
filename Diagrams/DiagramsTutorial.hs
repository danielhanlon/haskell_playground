{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

--circle1 = circle 1 # fc blue
--                   # lw 0.05
--                   # lc purple
--                   # dashing [0.2, 0.05] 0
--                   # pad 1.1

--circle1 = pad 1.1 . dashing [0.2, 0.05] 0 . lc purple . lw 0.05 . fc blue $ circle 1
circle1 = circle 1 # fc red # lw 0 ||| circle 1 # fc green # lw 0 # showOrigin

main = defaultMain circle1
