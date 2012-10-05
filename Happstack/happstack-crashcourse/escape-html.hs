module Main where

import Text.Html
import System.Environment

main =
    do [infp,outfp] <- getArgs 
       writeFile outfp . unlines . filter (not . null) . lines . prettyHtml . toHtml =<< readFile infp
