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
e2 = el1 || el1
  where el1 = circle 1 # scaleX 0.5 # rotateBy (1/6)

main = defaultMain e2
