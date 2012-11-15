atkin = do m <- [1..60]
   n <- [1..60]
   guard (mod (poly m n) 60 == k)
   return $
      do j <- [0..]
         let y = n + 60*j
         return $
            do i <- [0..]
               let x = m + 60*i
               guard (test x y)
               return (poly x y)
