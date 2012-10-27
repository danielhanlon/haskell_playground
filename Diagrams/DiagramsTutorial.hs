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
e2 = el1 ||| el1 
  where el1 = circle 1 # scaleX 0.5 # rotateBy (1/6) # pad 1.1

c = circle 1 # lw 0.1

circles = hcat' with {sep = 0.5}
          [ c
          , c # scale 2
          , c # freeze # scale 2
          , c # scaleX 0.2
          , c # freeze # scaleX 0.2
          ]
          # centerXY
          # pad 1.1

main = defaultMain circles
