--From Haskell tutorial on wikibooks.org
π=pi

--(²) :: Int -> Int
--x(²) = x*x
squares = [x^2 | x <- [1..5]]

areaRect l w = l*w
areaSquare s = areaRect s s

--Volume of a cylinder = pi*r^2*h
volumeCyl r h = pi*r*r*h

areaTriangleTrig a b c = c*height/2
  where
  cosA=(b^2+c^2-a^2)/(2*b*c)
  sinA=sqrt(1-cosA^2)
  height=b*sinA

areaTriangleHeron a b c = result
  where
  result = sqrt(s*(s-a)*(s-b)*(s-c))
  s = (a+b+c)/2

absolute x
  | x<0 = 0-x
  | otherwise = x

quadSolutions a b c
  | disc>0 = 2
  | disc==0 = 1
  | otherwise = 0
    where
    disc=b^2-4*a*c

data Maybe a = Just a
              | Nothing
