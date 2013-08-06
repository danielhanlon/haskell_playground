\begin{code}
{-# Language NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

sierpinski 1 = eqTriangle 1
sierpinski n =          s
                       ===
                   ( s ||| s ) # centerX
   where s = sierpinski (n-1)

main = defaultMain $ sierpinski 7
\end{code}
