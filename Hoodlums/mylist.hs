mylist = do
        a <- [1..]
        b <- [1..]
        c <- [1..]
        let aa=a*a
        let bb=b*b
        let cc=c*c
          guard ( aa+bb==cc )
        return (a,b,c)
