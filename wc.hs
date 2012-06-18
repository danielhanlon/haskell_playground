-- file: ch01/wc.hs
-- comment line

main = interact wordCount where wordCount input = show (length input) ++ "\n"
