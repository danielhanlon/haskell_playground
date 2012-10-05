module Main where

import Control.Monad (msum, mzero)
import Happstack.Server 
import Data.Char (toLower)

data Subject = World | Haskell

sayHello :: Subject -> String
sayHello World = "Hello, World!"
sayHello Haskell = "Greetings, Haskell!"

instance FromReqURI Subject where
  fromReqURI sub =
    case map toLower sub of
      "haskell" -> Just Haskell
      "world" -> Just World
      _ -> Nothing

main :: IO ()
main = simpleHTTP nullConf $ msum [ mzero
                                  , dir "hello" $ path $ \s -> ok $ (sayHello s)
                                  , dir "goodbye" $ path $ \s -> ok $ "Goodbye " ++ s
                                  ]
