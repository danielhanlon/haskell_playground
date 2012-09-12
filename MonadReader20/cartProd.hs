cartProd (set:sets) = [x:xs | x<-set, xs<-cp]
                      where cp=cartProd sets
cartProd [] = [[]]

data Point2D = Cartesian2D Double Double
             | Polar2D Double Double
             deriving (Eq,show)

instance Eq Point2D where
  Cartesian2D x y == Polar2D r theta = 
      x == rx && y == ry
          where rx=r
                ry=theta
