data Term = Con Float | Div Term Term
data Value a = Result a

instance Show a => Show (Value a) where
         show (Result x) = "Result: " ++ show x

eval1 :: Term -> Value Float
eval1 (Con x) = Result x
eval1 (Div t u) = Result (x/y)
                  where Result x = eval1 t
                        Result y = eval1 u

--Evaluation with exception handling (without Monads)
data Maybe a = Nothing | Just a
instance Show a => Show (Main.Maybe a) where
         show Main.Nothing = "Nothing"
         show (Main.Just x) = "Just "++show x

eval2 :: Term -> Main.Maybe Float
eval2 (Con x) = Main.Just x
eval2 (Div t u) = case eval2 t of
                  Main.Nothing -> Main.Nothing
                  Main.Just x -> case eval2 u of
                            Main.Nothing -> Main.Nothing
                            Main.Just y -> if y==0
                                      then Main.Nothing
                                      else Main.Just (x/y)

