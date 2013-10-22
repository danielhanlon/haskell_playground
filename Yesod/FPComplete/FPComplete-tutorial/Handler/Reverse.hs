module Handler.Reverse where

import Import
import Data.List (reverse)

getReverseR :: String -> Handler Html
getReverseR theText =
    defaultLayout $ do
        $(widgetFile "reverse")
