selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

permutations :: [a] -> [[a]]
permutations xs =
    [ y : zs
    | (y,ys) <- selections xs
    , zs     <- permutations ys
    ]
