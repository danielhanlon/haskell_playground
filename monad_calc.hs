--instance Monad Maybe where
--  return = Just
--  Just x >>= q = q x
--  Nothing x >>= q = Nothing

data Term = Con Float | Div Term Term
data Value a = Result a
instance Show a => Show (Value a) where
         show (Result x) = "Result: " ++ show x

eval1 :: Term -> Maybe Float
eval1 (Con x) = return x
eval1 (Div t u) = do x <- eval1 t
                     y <- eval1 u
                     if y/=0 then return (x/y) else Nothing
