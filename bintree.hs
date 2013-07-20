data Tree = Leaf Int | Branch Int Tree Tree

add :: Tree -> Int
add (Leaf i) = i
add (Branch i t1 t2) = i + add t1 + add t2

main = do
  let sum = add $
    Branch 4
      (Leaf 2) (Branch 4
                 (Branch 2
                    Leaf 3 Leaf 2) Leaf 5)
  putStrLn sum
