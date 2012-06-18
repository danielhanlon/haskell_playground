class BasicEq a where
  isEqual :: a -> a -> Bool
  isEqual x y = not (isNotEqual x y)
  isNotEqual :: a -> a -> Bool
  isNotEqual x y = not (isEqual x y)

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

instance BasicEq [Char] where
  isEqual [] [] = True
  isEqual (x:xs) (y:ys) 
    | (x/=y)    = False
    | otherwise = isEqual xs ys
  isEqual [] _  = False
  isEqual _ []  = False

--Demonstrating Show typeclass
data Color = Red | Green | Blue
instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"
