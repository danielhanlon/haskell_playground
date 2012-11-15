comb = do
  height <- [1..]
  a <- [1..height-1]
  let b = height-a
  return (a,b)

height h = do
  total <- [1..h]
  a <- [1..total-1]
  let b = total-a
  return (a,b)

triple h = ()
--  let (a,b) = take 
