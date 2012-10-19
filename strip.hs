stripCR :: [Char] -> [Char]
stripCR [] = []
--stripCR (a:as) = (a:(stripCR as))	, a='\n'
--               = stripCR as			, otherwise
stripCR (a:as) | a=='\n' 		= stripCR as
			   | otherwise 		= a:stripCR as