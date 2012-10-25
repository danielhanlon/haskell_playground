{-# Language NoMonomorphismRestriction #-}

sierpinski 1 = eqTriangle 1
sierpinski n =          s
                       ===
                   ( s ||| s ) # centerX
   where s = sierpinski (n-1)


