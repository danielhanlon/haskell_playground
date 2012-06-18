import Data.List (sortBy)

divide :: Float -> Float -> Float
divide x y = x/y
infixr `divide`

type Position = (Float, Float)
(€) :: Num a => a -> a
(€) a = 2*a

--Ex 1, 2
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1+myLength xs

--Ex 3
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (a:as) = a+mySum as

myMean :: (Integral a, Fractional a1) => [a] -> a1
myMean list
    | l==0 = undefined
    | otherwise  = s / l
    where
      s=fromIntegral (mySum list)
      l=fromIntegral (myLength list)

--Ex 4
palindromize :: [a] -> [a]
palindromize [] = []
palindromize list = list ++ tsil
    where
      tsil = reverse list

--Ex 5
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = undefined
isPalindrome (x:[]) = True
isPalindrome (x:xs) = do
    if x == (last xs)
        then isPalindrome (tail (reverse xs))
        else False

--Ex 6
sortList :: Ord a => [[a]] -> [[a]]
sortList [] = []
sortList a = sortBy sortLT a
  where
    sortLT a1 a2
      | a1l > a2l = GT
      | otherwise = LT
      where
        a1l = length a1
        a2l = length a2

--Ex 7
--intersperse :: a -> [[a]] -> [a]
intersperse s [] = ""
intersperse s (e:[]) = e
intersperse s (x:xs) = x ++ s ++ intersperse s xs

--Ex8
--Binary tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)
data ExTree a = Leaf a
              | Branch [ExTree a]
              deriving (Show)
travTree (Leaf l) = l
travTree (Branch branches) = concat $ map travTree branches
--Fail - copied from answers...
height :: Tree t -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)

--Ex 9,10
data Direction = TurnLeft | TurnRight | StraightOn 
  deriving (Show)
whichDirection :: (Ord a, Num a) => (a,a) -> (a,a) -> (a,a) -> Direction
whichDirection (ax,ay) (bx,by) (cx,cy) 
    | det  == 0 = StraightOn
    | det  > 0 = TurnLeft
    | det  < 0 = TurnRight
      where
        det = a*d-b*c
        a = bx-ax
        b = by-ay
        c = cx-bx
        d = cy-by

--Ex 11
path (a:b:c:ps) = (whichDirection a b c):(path (b:c:ps))
path _ = []

--Chapter 6
data Colour = Red | Green | Blue


