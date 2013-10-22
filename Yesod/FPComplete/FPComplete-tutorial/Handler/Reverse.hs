module Handler.Reverse where

import Import
import Data.List (reverse)

getReverseR :: text -> Handler Html
getReverseR theText =
    defaultLayout $ [whamlet|<h1>The text reversed is:#{reverse theText}|]
