module Main where

import Control.Monad (msum)
import Data.Char (toLower)
import Happstack.Server (FromReqURI(..), nullConf, simpleHTTP
                        , ok, dir, path)

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

main = IO ()
main = simpleHTTP nullConf $ dir "hello"
          $ path $ \subject -> ok $ (sayHello subject)
