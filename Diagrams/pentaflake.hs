{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude

pentaflake' 0 = regPoly 5 1 # lw 0
pentaflake' n = appends
		 pCenter
		 (zip vs (repeat (rotateBy (1/2) pOutside)))
	where vs = iterateN 5 (rotateBy (1/5))
		 . (if odd n then negateV else id)
		 $ unitY
	       pCenter = pentaflake' (n-1)
	       pOutside = pCenter # opacity 0.8

pentaflake n = pentaflake' n # fc blue # bg white

main = defaultMain ( pad 1.1 $ pentaflake 4 )
