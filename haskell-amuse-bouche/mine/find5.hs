getListOrig = find 5 where
     find 0 = return []
     find n = do
       ch <- getChar
       if ch `elem` ['a'..'e'] then do
          tl <- find (n-1)
          return (ch : tl) else
        find n


getList :: IO [Char]
getList = fmap take5 getContents

take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])

