{-# LANGUAGE EmptyDataDecls, TypeOperators #-}

data Unit = Unit
-- ()


--Need TypeOperators pragma for the :+ below
data a :+ b = AddL a | AddR b
-- Either

data a :* b = Mul a b
-- (a,b)

--Need EmptyDataDecls pragma for below
data Void

type Two = Unit :+ Unit
-- Bool

-- a->b <=> b^a
--
-- The derivative of a regular type is its type of One-Hole contexts
