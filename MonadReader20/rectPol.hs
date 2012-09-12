---rectPolh.hs
data Point2D = Cartesian2D Double Double
             | Polar2D Double Double
             deriving (Show)

instance Eq Point2D where
  Cartesian2D x y == Polar2D r theta = 
      distance (x,y) (rx,ry) < 0.01
          where rx=r * cos theta
                ry=r * sin theta
                distance (x,y) (rx,ry) = sqrt ((rx-x)*(rx-x) + (ry-y)*(ry-y))
