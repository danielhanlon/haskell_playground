comb = do
  height <- [1..]
  a <- [1..height-1]
  let b = height-a
  return (a,b)
