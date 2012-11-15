mylist = do
        a <- [1..]
        b <- [1..]
        c <- [1..]
        guard (A + B == C)
          where A=a*a
                B=b*b
                C=c*c
