module Main () where

import JSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
