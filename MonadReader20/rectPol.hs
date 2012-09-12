---rectPolh.hs
data Point2D = Cartesian2D Double Double
             | Polar2D Double Double
             deriving (Show)

instance Eq Point2D where
  Cartesian2D x y == Polar2D r theta = 
      x == rx && y == ry
          where rx=r * cos theta
                ry=r * sin theta
