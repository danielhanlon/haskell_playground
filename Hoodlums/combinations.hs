comb = do
  height <- [0..]
  a <- [0..height-2]
  b <- [0..height-2]
  let c = height-(a+b)
  return (a,b,c)
