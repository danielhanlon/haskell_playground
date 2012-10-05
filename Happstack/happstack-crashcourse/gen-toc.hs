module Main where

import Control.Applicative
import Data.List (groupBy, isSuffixOf)
import Data.Function (on)
-- import Data.Tree
import Text.HTML.TagSoup
import Text.Html
import System.Environment (getArgs)

isOpenHeader t =
    t ~== "<h1>" || t ~== "<h2>" || t ~== "<h3>" || t ~== "<h4>" || t ~== "<h5>"

isCloseHeader t =
    t ~== "</h1>" || t ~== "</h2>" || t ~== "</h3>" || t ~== "</h4>" || t ~== "</h5>"

data Header = Header { level  :: Int 
                     , aname :: Maybe String 
                     , label  :: String
                     , file   :: FilePath
                     } deriving (Eq, Ord, Read, Show)

extractHeader :: FilePath -> [Tag String] -> Header
extractHeader fp (TagOpen hStr _:ts) =
    let lvl = case hStr of
                "h1" -> 1
                "h2" -> 2
                "h3" -> 3
                "h4" -> 4
                "h5" -> 5
    in extractName lvl ts
    where
      extractName lvl (TagOpen "a" attrs: ts) =
          case lookup "name" attrs of
            Nothing -> extractLbl lvl Nothing ts
            (Just n) -> extractLbl lvl (Just n) ts
      extractName lvl ts = extractLbl lvl Nothing ts
      extractLbl lvl n ts = Header lvl n (innerText ts) fp

getHeaders :: FilePath -> IO [Header]
getHeaders fp =
    do c <- fmap parseTags $ readFile fp
       let headers = map (extractHeader fp . takeWhile (\t -> not (isCloseHeader t))) $ sections isOpenHeader c
       return headers

main :: IO ()
main =
    do files <- filter (isSuffixOf ".html") <$> getArgs
       hs <- fmap concat $ mapM getHeaders files
       let htree = map snd $ toTree (map headerToPair hs)
       print (forestToHtml htree)

forestToHtml :: [Tree Header] -> Html
forestToHtml [] = noHtml
forestToHtml trees = olist $ concatHtml (map treeToHtml trees)

treeToHtml :: Tree Header -> Html
treeToHtml (Tree header children)  = li $ headerToHtml header +++ forestToHtml children

headerToHtml :: Header -> Html
headerToHtml h = (anchor (toHtml (label h))) ! [href ((file h) ++ (maybe "" ("#"++) (aname h))) ]

headerToPair :: Header -> (Int, Header)
headerToPair h = (level h, h)


data Tree a = Tree a [Tree a] deriving Show

toTree :: Show a => [(Int, a)] -> [(Int, Tree a)]
toTree pairs =
    f [] (reverse pairs)
    where
      f :: Show a => [(Int, Tree a)] -> [(Int, a)] -> [(Int, Tree a)]
      f [] ((d', x) : more) = f [(d', Tree x [])] more
      f stack [] = stack
      f stack@((d, trees) : _) pairs@((d', x) : more) =
          case compare d (d' + 1) of
            -- If the new pair depth is one less than the stack top depth, pair becomes new parent
            EQ -> let (children, other) = break (\ (d, _) -> d /= d' + 1) stack in f  ((d', Tree x (map snd children)) : other) more
            -- If the new pair depth is less than or equal to the stack top depth, push it
            LT -> f ((d', Tree x []) : stack) more
            -- If the new pair depth is greater than the stack top depth, it is an error.
            GT -> error $ "Invalid traversal: stack=" ++ show stack ++ ", pairs=" ++ show pairs
    
{-
toForest :: Int -> [Header] -> Forest Header
toForest _ [] = []
toForest currLvl hs@(h@(Header lvl _ _) : rest) | 
-}



