comb = do
  height <- [1..]
  a <- [1..height-2]
  b <- [1..height-2]
  let c = height-(a+b)
  return (a,b,c)
