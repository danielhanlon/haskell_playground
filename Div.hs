import Data.Maybe

(//) :: Maybe Float -> Maybe Float -> Maybe Float
_ // Just 0 = Nothing
Just x // Just y = Just (x/y)
_ // _ = Nothing

par :: Float -> Float -> Maybe Float
par R1 R2 = Maybe 1 //(( Maybe 1 // Maybe R1 )+( Maybe 1 // Maybe R2 ))
