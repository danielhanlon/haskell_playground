data Tree = Leaf Int | Branch Int Tree Tree

add :: Tree -> Int
add Leaf i = i
add Branch i t1 t2 = i + add t1 + add t2
