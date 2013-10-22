module Handler.Reverse where

import Import
import Data.List (reverse)

getReverseR :: Text -> Handler Html
getReverseR theText =
    defaultLayout $ [whamlet|<h1>The text reversed is:#{reverse theText}|]
