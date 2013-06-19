{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main = defaultMain $ circle 1 # fc red # lw 0 ||| circle 1 # fc green # lw 0
