comb = do
  height <- [1..]
  a <- [1..height-3]
  b <- [1..height-3]
  let c = height-(a+b)
  return (a,b,c)
